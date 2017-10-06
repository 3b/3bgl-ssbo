(in-package #:3bgl-ssbo)

;;; code walker copied from 3bgl-shader, with extra bits removed
;; todo: clean this stuff up/improve API (maybe split to separate project)

(defparameter *verbose* nil "enable debugging printouts")

(defclass walker ()
  ())

(defgeneric walk (form walker))
(defgeneric walk-cons (car cdr walker))

(defmethod walk (form walker)
  form)

(defmethod walk ((form cons) walker)
  (walk-cons (car form) (cdr form) walker))

(defmethod walk-cons (car cdr walker)
  (list* car (mapcar (lambda (f) (walk f walker)) cdr)))

(defvar *environment* nil "current local environment")


;; todo: store declarations somewhere in env, and parse them when walking code?
(defclass environment ()
  ((parent-scope :reader parent-scope :initarg :parent :initform nil)
   ;; map of name -> FUNCTION-BINDING instance
   (function-bindings :reader function-bindings :initform (make-hash-table))
   ;; name -> MACRO-DEFINITION instance
   (compiler-macro-bindings :reader compiler-macro-bindings :initform (make-hash-table))
   ;; map of name -> BINDING instance
   (variable-bindings :reader variable-bindings :initform (make-hash-table))
   (name :reader name :initform nil :initarg :name)))

(defun get-variable-binding (name &key (env *environment*))
  (and env
       (or (gethash name (variable-bindings env))
           (get-variable-binding name :env (parent-scope env)))))

(defun get-function-binding (name &key (env *environment*))
  (when env
    (or (gethash name (function-bindings env))
        (get-function-binding name :env (parent-scope env)))))

(defun get-compiler-macro-binding (name &key (env *environment*))
  ;; find a compiler macro in any env no deeper than current
  ;; function-binding
  ;; (function bindings shadow compiler macro bindings)
  (when env
    (or (gethash name (compiler-macro-bindings env))
        (when (gethash name (function-bindings env))
          ;; don't use a compiler macro from a parent scope if we have a
          ;; function definition in this scope
          (return-from get-compiler-macro-binding nil))
        (get-compiler-macro-binding name :env (parent-scope env)))))

(defun add-macro (name lambda &key (env *environment*))
  (setf (gethash name (function-bindings env))
        (compile 'nil lambda)))

(defun add-compiler-macro (name lambda &key (env *environment*))
  (setf (gethash name (compiler-macro-bindings env))
        (compile 'nil lambda)))

(defun get-compiler-macro-function (name)
  (let ((mf (get-compiler-macro-binding name :env *environment*)))
    (when (typep mf 'function)
      mf)))

(defun get-macro-function (name)
  (let ((mf (get-function-binding name :env *environment*)))
    (cond
      ((typep mf 'function)
       mf)
      ((not mf) ;; no local functions, use global binding
       (macro-function name)))))

(defun get-symbol-macro (name)
  (let ((b (get-variable-binding name :env *environment*)))
    (cond
      ((typep b '(cons (eql :macro)))
       b)
      ((and (not b) (not (eql name (macroexpand name nil))))
       (list 'symbol-macro :name name :expansion (macroexpand name))))))

(defun add-symbol-macro (name expansion &key (env *environment*))
  (assert (not (gethash name (variable-bindings env))))
  (setf (gethash name (variable-bindings env))
        (list 'symbol-macro
              :name name
              :expansion expansion)))

(defun add-variable (name init &key (env *environment*) (type :variable-binding))
  (setf (gethash name (variable-bindings env))
        (list type name init)))


(defun make-function-bindings (lambda-list)
  (multiple-value-bind (req opt rest key aux)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignorable req))
    (when rest
      (error "&rest not supported"))
    (when aux
      (error "&aux not supported yet"))
    ;; if we have any supplied-p args, we just expand into all required args
    ;; else if we have only optional args, we let glsl default values handle it
    ;; else if we have &key, we expand to optional positional args
    (let ((use-optional (not (or (some 'third opt) (some 'third key))))
          (args (reverse req))
          (bindings (mapcar (lambda (n)
                              (list 'function-argument
                                    :name n
                                    :init nil))
                            (reverse req))))
      (loop for (n i s) in opt
            when n
              do (push n args)
                 (push (list 'function-argument
                             :name n
                             :init (when use-optional i))
                       bindings)
            when s
              do (push `(if ,s 1 0) args)
                 (push (list 'function-argument
                             :name s
                             :init nil)
                       bindings))

      (loop for ((nil n) i s) in key
            when n
              do (push n args)
                 (push (list 'function-argument
                             :name n
                             :init (when use-optional i))
                       bindings)
            when s
              do (push `(if ,s 1 0) args)
                 (push (list 'function-argument
                             :name s
                             :init nil)
                       bindings))
      (reverse bindings))))

(defun add-function (name lambda-list body
                     &key declarations docs (env *environment*)
                       (function-type 'global-function))
  (when *verbose* (format t "add function ~s~%" name))
  (multiple-value-bind (bindings)
      (make-function-bindings lambda-list)
    (setf (gethash name (function-bindings env))
          (list function-type
                :name name
                :lambda-list lambda-list
                :bindings bindings
                :body body
                :docs docs
                :declarations declarations))))

(defun add-function-arguments (function &key (env *environment*))
  ;; add the bindings from a functions's arglist to current environment
  (loop with v = (variable-bindings env)
        for b in (getf (cdr function) :bindings)
        for n = (name b)
        do (assert (not (gethash n v)))
           (setf (gethash n v) b)))

(defun remove-function (name &key (env *environment*))
  (remhash name (function-bindings env)))

(defclass cl-walker (walker)
  ())

(defmacro with-environment-scope (() &body body)
  `(let ((*environment* (make-instance 'environment :parent *environment*)))
     ,@body))



(defmacro defwalker (walker (form &rest args) &body body)
  (alexandria:with-gensyms (car cdr)
    `(defmethod walk-cons ((,car (eql ',form)) ,cdr (walker ,walker))
       (declare (ignorable ,car))
       (labels ((@ (form) (walk form walker))
                (@@ (forms &key declare)
                    (if (and declare (typep (car forms)
                                            '(cons (member declare))))
                        (cons (car forms) (mapcar #'@ (cdr forms)))
                        (mapcar #'@ forms))))
         (declare (ignorable #'@ #'@@))
         (destructuring-bind ,args ,cdr
           ,@body)))))



;; +block     +let*                 +return-from
;; +catch     +load-time-value      +setq
;; +eval-when +locally              +symbol-macrolet
;; +flet      +macrolet             +tagbody
;; +function  +multiple-value-call  .the
;; +go        +multiple-value-prog1 +throw
;; .if        .progn                unwind-protect
;; +labels     progv
;; +let        quote
;;

;; same as default behavior:
;;  if, progn, the

(defwalker cl-walker (block name &rest body)
  ;; probably should add blocks to environment?
  `(block ,name ,@(@@ body)))

(defwalker cl-walker (return-from name &optional (result nil resultp))
  `(return-from ,name ,@(when resultp (list (@ result)))))

(defwalker cl-walker (catch tag &rest forms)
  `(catch ,(@ tag) ,@(@@ forms)))

(defwalker cl-walker (throw tag result-form)
  `(throw ,(@ tag) ,(@ result-form)))

(defwalker cl-walker (load-time-value form &optional read-only-p)
  `(load-time-value ,(@ form) ,read-only-p))

(defwalker cl-walker (setq &rest assignments)
  `(setq ,@(loop for (a b) on assignments by #'cddr
                 when (nth-value 1 (get-symbol-macro a))
                   do (error "can't expand assignment to symbol macros in SETQ yet (in form ~s)" `(setq ,@assignments))
                      ;; fixme: implement symbol-macro stuff somewhere
                 collect a
                 collect (@ b))))

(defwalker cl-walker (eval-when (&rest situations) &body body)
  `(eval-when ,situations
     ,@(@@ body)))

(defwalker cl-walker (locally declarations &body body)
  `(locally ,declarations ,@(@@ body)))


(defwalker cl-walker (symbol-macrolet (&rest bindings) &rest body)
  ;; not sure if there is any reason to preserve the symbol-macrolet, so
  ;; just letting it expand for now...
  (with-environment-scope ()
    (loop for (name expansion) in bindings
          do (add-symbol-macro name expansion))
    (let ((w (@@ body :declare t)))
      (typecase w
        ((cons (member progn)) w)
        ((cons T NULL) (car w))
        ;; fixme: why doesn't 'implicit-progn work here?
        (t (list 'explicit-progn :body w))))))

(defwalker cl-walker (macrolet (&rest bindings) &rest body)
  ;; not sure if there is any reason to preserve the macrolet, so
  ;; just letting it expand for now...
  (with-environment-scope ()
    (loop for (name lambda-list . body) in bindings
          do (add-macro name
                        `(lambda (form env)
                           (declare (ignorable env))
                           (destructuring-bind ,lambda-list
                               (cdr form)
                             ,@body))))
    (let ((w (@@ body :declare t)))
      (typecase w
        ((cons (member progn)) w)
        ((cons T NULL) (car w))
        (t (cons 'progn w))))))

(defwalker cl-walker (tagbody &body body)
  ;; todo: probably should store go tags in environment
  `(tagbody
      ,@(loop for f in body
              when (consp f)
                collect (@ f)
              else collect f)))

(defwalker cl-walker (go tag)
  `(go ,tag))


(defmacro with-lambda-list-vars ((function) &body body)
  `(with-environment-scope ()
     (mapcar (lambda (a) (add-variable (name a) nil :binding a))
             (bindings ,function))
     ,@body))

(defun walk-function-body (walker lambda-list body)
  ;; fixme: not sure if this gets the scopes quite right when same
  ;; name appears multiple times
  ;; not sure it matters though, since anything that cares probably
  ;; has its own code walker?
  (let* ((declare (when (typep (car body) '(cons (member declare)))
                    (pop body)))
         (walked
           (with-environment-scope ()
             (mapcar #'(lambda (a) (add-variable a nil))
                     (lambda-list-vars lambda-list))
             (mapcar (lambda (a) (walk a walker)) body))))
    (if declare
        (cons declare walked)
        walked)))

(defwalker cl-walker (function name)
  (if (typep name '(cons (member lambda)))
      `(function
        (lambda (second name)
         ,(walk-function-body walker (cadr name) (cddr name))))
      `(function name)))

(defwalker cl-walker (multiple-value-call function-form form*)
  `(multiple-value-call ,(@ function-form) ,@(@@ form*)))

(defwalker cl-walker (multiple-value-prog1 first-form form*)
  `(multiple-value-prog1 ,(@ first-form) ,@(@@ form*)))

(defwalker cl-walker (unwind-protect protected-form &rest cleanup-form*)
  `(unwind-protect ,(@ protected-form)
     ,@(@@ cleanup-form*)))

(defwalker cl-walker (progv symbols values &rest form*)
  ;; fixme: should we add dynamic bindings to environment?
  `(progv
       ,(@ symbols)
       ,(@ values)
     ,@(@@ form*)))

(defwalker cl-walker (quote object)
  ;; fixme: should we add dynamic bindings to environment?
  `(quote ,object))


(defwalker cl-walker (let (&rest bindings) &rest body)
  ;; walk default values if any, and body
  (let ((previous (make-hash-table)))
    `(let (,@(mapcar (lambda (a)
                       (let ((var (if (consp a) (car a) a))
                             (init (if (consp a) (cadr a) nil)))
                         (setf (gethash var previous) t)
                         (list var (@ init))))
                     bindings))
       ,@(with-environment-scope ()
           (mapcar (lambda (a) (add-variable (if (consp a) (car a) a) nil))
                   bindings)
           (@@ body :declare t)))))

(defwalker cl-walker (let* (&rest bindings) &rest body)
  ;; walk default values if any, and body
  (with-environment-scope ()
    `(let* (,@(mapcar (lambda (a)
                        (let ((var (if (consp a) (car a) a))
                              (init (if (consp a) (cadr a) nil)))
                          (prog1
                              (list var (@ init))
                            (add-variable a nil))))
                      bindings))
       ,@(@@ body :declare t))))

(defun lambda-list-vars (lambda-list)
  (multiple-value-bind (req opt rest keys aux)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (remove 'nil
            (append req
                    (mapcar 'car opt) (mapcar 'third opt)
                    (list rest)
                    (mapcar 'cadar keys)
                    (mapcar 'third keys)
                    (mapcar 'first aux)))))

(defwalker cl-walker (multiple-value-bind vars form &rest body)
  `(multiple-value-bind ,vars ,(@ form)
     ,@ (with-environment-scope ()
          (loop for name in vars
                do (add-variable name nil))
          (@@ body))))

(defwalker cl-walker (flet (&rest functions) &rest body)
  (declare (ignore functions body))
  ;; walk function bodies (with local functions not in scope yet)
  (error "rewrite this...")
  #++
  (let ((walked (loop for (f ll . body+d) in functions
                      for (body declare doc) = (multiple-value-list
                                                (alexandria:parse-body
                                                 body+d :documentation t))
                      for walked = (with-lambda-list-vars (walker ll)
                                     (@@ body))
                      collect (list f ll walked declare doc))))
    ;; then rebuild expanded flet form
    `(flet (,@(mapcar (lambda (a)
                        (destructuring-bind (f ll wbody declare doc) a
                          `(,f ,ll
                               ,@(when declare `(,declare))
                               ,@(when doc `(,doc))
                               ,@wbody)))
                walked))
       ,@(with-environment-scope (walker)
           ;; add local functions to env
           (mapcar (lambda (a)
                     (destructuring-bind (f ll wbody declare doc) a
                       (add-function :local-function f ll wbody declare doc)))
                   walked)
           ;; and walk main body
           (@@ body :declare t)))))

(defwalker cl-walker (labels (&rest functions) &rest body)
  (declare (ignore functions body))
  ;; walk function bodies and main body
  (error "rewrite this...")
  #++
  (with-environment-scope (walker)
    ;; add all function names to env (with empty bodies for now)
    ;; so they are in scope while walking bodies (even if we can't
    ;; have recursive functions, they still should shadow
    ;; any enclosing macro definitions and such)
    (mapcar (lambda (a)
              (add-function :local-function (first a) (second a) nil nil nil))
            functions)
    ;; walk function bodies, and update definitions in env
    `(labels (,@(loop for (f ll . body+d) in functions
                      for (body declare doc) = (multiple-value-list
                                                (alexandria:parse-body
                                                 body+d :documentation t))
                      for walked = (with-lambda-list-vars (walker ll)
                                     (@@ body))
                      ;; fixme: figure out how to do this without env internals
                      for fenv = (gethash f (function-bindings *environment*))
                      do (setf (getf (cdr fenv) :body) walked)
                         (setf (getf (cdr fenv) :declare) declare)
                         (setf (getf (cdr fenv) :doc) doc)
                      collect `(,f ,ll
                                   ,@(when declare (list declare))
                                   ,@(when doc (list doc))
                                   ,@walked)))
       ;; and walk main body
       ,@(@@ body :declare t))))


(defmethod walk-cons (car cdr (walker cl-walker))
  (let* ((macro (get-macro-function car))
         (cmacro (unless macro
                   (get-compiler-macro-function car)))
         (form (list* car cdr)))
    (cond
      (cmacro
       (let ((expanded (funcall cmacro form *environment*)))
         (if (eq expanded form)
             (if macro
                 (walk (funcall macro form *environment*) walker)
                 (call-next-method))
             (walk expanded walker))))
      (macro
       (walk (funcall macro form *environment*) walker))
      (t
       (call-next-method)))))

(defmethod walk (form (walker cl-walker))
  ;; expand symbol macros
  (multiple-value-bind (macro mp) (get-symbol-macro form)
    (if mp
        (walk macro walker)
        (call-next-method))))

(defparameter *depth* 1)
(defparameter *max-depth* 100)

(defmethod walk :around (form walker)
  (let ((*depth* (1+ *depth*)))
    (when (> *depth* *max-depth*)
      (error "nesting depth too deep"))
    (call-next-method)))

(defmethod walk-cons :around (car cdr walker)
  (let ((*depth* (1+ *depth*)))
    (when (> *depth* *max-depth*)
      (error "nesting depth too deep"))
    (call-next-method)))

