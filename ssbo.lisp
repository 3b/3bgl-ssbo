(in-package #:3bgl-ssbo)

(defvar *writer-structs*)
(defvar *writer-defaults*)
(defvar *writer-functions*) ;; hash of type -> (write-form local-function-def)

(defclass ssbo ()
  ((buffer :accessor buffer :initform nil)
   (dirty :accessor dirty :initform t)
   (packing :accessor packing :initform nil :initarg :packing)
   (ssbo-size :accessor ssbo-size :initform nil)
   (writer :accessor writer :initform nil)
   ;; if SSBO has variable sized array as last element, optionally
   ;; write # of entries into specified slot of top-level block.
   (count-slot :accessor count-slot :initform nil :initarg :count-slot)
   ;; todo: support more than just hash tables
   (data :accessor data :initform (make-hash-table) :initarg :data)))

(defun make-ssbo (&rest args &key packing index name count-slot data)
  (declare (ignore packing index name count-slot data))
  (apply #'make-instance 'ssbo args))

(defmethod update-ssbo-layout (ssbo layout)
  (setf (packing ssbo) layout)
  ;; todo: implement defaults for var-sized array entries
  (let ((*writer-defaults* (make-hash-table)))
    (setf (writer ssbo) (make-writer-for-layout (packing ssbo)
                                                (count-slot ssbo))))
  (setf (dirty ssbo) t))

(defmethod bind-ssbo ((ssbo ssbo) index)
  (when (and (packing ssbo)
             (writer ssbo))
    ;; make sure buffer is current
    (let* ((packing (packing ssbo))
           (variable-size-field (variable-size-field (getf packing :packing)))
           (data (data ssbo))
           (count (length (gethash variable-size-field data)))
           (base (getf packing :base))
           (stride (or (getf packing :stride) 0))
           (buffer-size (+ base (* count stride)))
           (resize (not (and (buffer ssbo)
                             (eql buffer-size (ssbo-size ssbo))))))
      ;; need to upload data
      (when (or (dirty ssbo) resize)
        ;; (re)create buffer if needed
        (when resize
          (when (buffer ssbo)
            (gl:delete-buffers (list (shiftf (buffer ssbo) nil))))
          (setf (buffer ssbo) (gl:create-buffer))
          (gl:named-buffer-storage (buffer ssbo) nil '(:dynamic-storage)
                                   :end buffer-size)
          (setf (ssbo-size ssbo) buffer-size))
        (cffi:with-foreign-object (p :char buffer-size)
          (funcall (writer ssbo) (data ssbo) p buffer-size)
          (%gl:named-buffer-sub-data (buffer ssbo) 0 (ssbo-size ssbo) p))))
    ;; bind buffer
    (when (buffer ssbo)
      (%gl:bind-buffer-base :shader-storage-buffer index (buffer ssbo)))))

(defun get-buffer-for-binding (ssbos index)
  (car
   (find-if (lambda (a)
              (eql (getf (getf (cddr a) :layout) :binding)
                   index))
            ssbos)))

(defun calculate-layout (ssbos structs &key index name)
  (let* (
         (buffer-name (if name
                          name
                          (get-buffer-for-binding ssbos (or index 0)))))
    (when buffer-name
      (let ((pack ()))
        (loop for (name nil . rest) in structs
              do (push `(,name
                         (:struct
                             ()
                           ,@(glsl-packing:expand-glsl-types
                              (getf rest :components))))
                       pack))
        (loop for (name nil . rest) in ssbos
              when (getf (getf rest :layout) :std430)
                do (push '(:packing :std430) pack)
              do (push `(,name
                         (:block
                             ()
                           ,@(glsl-packing:expand-glsl-types
                              (getf rest :components))))
                       pack))
        (let* ((packed (glsl-packing:pack-structs
                        (nreverse pack) :roots (list buffer-name)))
               (ssbo (assoc buffer-name packed :key 'car))
               ;; we assume the material ssbo ends with variable-size
               ;; member, so return size of initial part of block and
               ;; stride of elements in the last member
               (type (second ssbo))
               (last (car (last (getf type :members))))
               (struct (assoc (cadar (getf last :type)) packed :key 'car)))
          (list :packing type
                :type buffer-name
                :struct (car struct)
                :types (alexandria:plist-hash-table
                        (loop for ((n) p) in packed
                              collect n collect (list* :struct p)))
                :base (getf type :size)
                :stride (getf last :stride)))))))

(defparameter *inv-base-types*
  (alexandria:plist-hash-table
   (reverse (alexandria:hash-table-plist glsl-packing:*base-types*))
   :test 'equalp))
(cffi:defctype bool :int32)
(defparameter *foreign-type-map*
  (alexandria:plist-hash-table
   '((:float 32) :float
     (:float 64) :double
     ;; (:float 16) ??
     (:int 8) :int8
     (:int 16) :int16
     (:int 32) :int32
     (:int 64) :int64
     (:uint 8) :uint8
     (:uint 16) :uint16
     (:uint 32) :uint32
     (:uint 64) :uint64
     (:bool) 'bool)
   :test 'equalp))

(defun variable-size-field (packing)
  (when (and (getf packing :stride)
             (plusp (getf packing :stride)))
    (let* ((pack (getf packing :packing))
           (members (getf pack :members))
           (last (car (last members)))
           (type (car (getf last :type))))
      (assert (eql (first type) :array))
      (assert (member (third type) '(* :*)))
      (getf last :name))))

(defun make-value-writer (type pointer value &key index slot)
  (unless (gethash (gethash type *writer-structs* type)
                   *writer-functions*)
    (let* ((orig-type type)
           (type (if (consp type)
                     type
                     (gethash type *writer-structs* type)))
           (ctype (gethash type *foreign-type-map*)))
      (if ctype
          (let ((fn (gensym (format nil "~@:(~a.~)" ctype))))
            (setf (gethash type *writer-functions*)
                  (list fn
                        `(,fn (pointer value &optional (index 0))
                             (setf (cffi:mem-aref pointer ,ctype index)
                                   ,(ecase (car type)
                                      (:bool
                                       `(if value 1 0))
                                      (:float
                                       (if (= (second type) 64)
                                           '(coerce value 'double-float)
                                           '(coerce value 'single-float)))
                                      (:int
                                       '(coerce value 'integer))
                                      (:uint
                                       '(coerce value 'unsigned-byte))))))))
          (ecase (car type)
            (:vec
             (let ((fn (gensym (format nil "~@:(vec~a/~a.~)"
                                       (third type)
                                       (gethash (second type)
                                                *foreign-type-map*
                                                (second type))))))
               (setf (gethash type *writer-functions*)
                     (list fn
                           `(,fn (pointer value)
                                ,@(loop for i below (third type)
                                        append (make-value-writer
                                                (second type)
                                                'pointer
                                                `(elt value ,i)
                                                :index i)))))))
            (:mat
             ;; accept either a 4x4 or MxN matrix
             (let ((fn (gensym (format nil "~@:(mat~ax~a/~a.~)"
                                       (third type)
                                       (fourth type)
                                       (gethash (second type)
                                                *foreign-type-map*
                                                (second type)))))
                   (element-count (* (third type) (fourth type))))
               (destructuring-bind (&key matrix-stride major
                                    &allow-other-keys)
                   slot
                 (declare (ignorable major matrix-stride)))
               (setf (gethash type *writer-functions*)
                     (list fn
                           `(,fn (pointer value)
                                (cond
                                  ((= (array-total-size value)
                                      ,element-count)
                                   ;; fixme: handle matrix-stride
                                   (loop for i below ,element-count
                                         do ,@(make-value-writer
                                               (second type)
                                               'pointer
                                               `(elt value i)
                                               :index 'i)))
                                  ((= (array-total-size value) 16)
                                   ;; todo: copy from upper-left of
                                   ;; column-major 4x4 array
                                   )
                                  #++
                                  ((= (array-rank value) 2)
                                   ;; todo: copy from upper-left of 2d array,
                                   ;; fill extra with identity
                                   )))))))
            (:array
             (let ((fn (gensym (format nil "~@:(array~a/~a.~)"
                                       (third type)
                                       (gethash (second type)
                                                *foreign-type-map*
                                                (second type))))))
               (setf (gethash type *writer-functions*)
                     ;; fixme: handle :* here instead of special case
                     ;; (needs to make sure it is last slot, and check
                     ;; pointer bounds)
                     (if (member (third type) '(:* *))
                         (list nil nil)
                         (list
                          fn
                          `(,fn (pointer value)
                               (--- array)))))))
            (:struct
                (let ((fn (gensym (format nil "~@:(struct/~a.~)" orig-type))))
                  (setf (gethash orig-type *writer-functions*)
                        (list fn
                              `(,fn (pointer value)
                                   ,@(make-slot-writers (getf (cdr type) :members)
                                                        'value 'pointer
                                                        *writer-defaults*))))))))))
  (let ((fn (car (gethash type *writer-functions*))))
    (when fn
      (list (list* fn pointer value (when index (list index)))))))

(defun default-for-type (slot)
  (let ((type (car (getf slot :type))))
    (or (gethash (getf slot :name) *writer-defaults*)
        (typecase type
          ((cons (eql :bool))
           nil)
          ((cons (member :int :uint))
           0)
          ((cons (eql :float))
           0.0)
          ((cons (eql :vec) (cons (cons (eql :int))))
           (subseq #(0 0 0 1) 0 (third type)))
          ((cons (eql :vec) (cons (cons (eql :float))))
           (subseq #(0.0 0.0 0.0 1.0) 0 (third type)))
          ((cons (eql :mat))
           #(1 0 0 0
             0 1 0 0
             0 0 1 0
             0 0 0 1))))))

(defun make-slot-writer (slot pointer value)
  (destructuring-bind (&key type offset &allow-other-keys) slot
    (make-value-writer (car type)
                       (if (plusp offset)
                           `(cffi:inc-pointer ,pointer ,offset)
                           pointer)
                       value
                       :slot slot)))

(defun make-slot-writers (slots value-hash pointer defaults-hash)
  (declare (ignore defaults-hash))
  (loop for slot in slots
        for name = (getf slot :name)
        when (make-slot-writer slot pointer
                               `(gethash ',name ,value-hash
                                         ,(default-for-type slot)))
          append it))

(defun make-writer-for-layout (layout count-slot)
  (let* ((top-slots (getf (getf layout :packing) :members))
         (struct-type (getf layout :struct))
         (base (getf layout :base))
         (stride (getf layout :stride))
         (*writer-structs* (getf layout :types))
         (*writer-functions* (make-hash-table :test 'equalp))
         (count-slot (and count-slot
                          (find count-slot top-slots
                                :key (lambda (a) (getf a :name))))))
    (let ((*package* (or (find-package '3bgl-ssbo) *package*))
          ;; calculate body first so we can add local functions around it
          (body `((assert (<= (+ ,base
                                  ,@ (when stride
                                       `((* ,stride (length entries)))))
                              size))
                  ,@(when count-slot
                      `((setf (gethash ',(getf count-slot :name) globals)
                              (length entries))))
                  ,@(make-slot-writers top-slots
                                       'globals 'pointer nil)
                  ,@(when (and struct-type (plusp stride))
                      `((loop for p2 = (cffi:inc-pointer pointer ,base)
                                then (cffi:inc-pointer p2 ,stride)
                              for mat across entries
                              for i from 0
                              do (progn
                                   ,@(make-value-writer (car struct-type)
                                                        'p2 'mat))))))))
      (if layout
          (let ((l `(lambda (globals pointer size &key entries)
                      (declare (ignorable entries))
                      (unless (zerop (mod (cffi:pointer-address pointer)
                                          ,(or (getf (getf layout :packing) :align)
                                               0)))
                        (break "align? ~s ~s" pointer
                               ,(getf (getf layout :packing) :align)))
                      (labels (,@ (remove nil
                                    (mapcar 'second
                                            (alexandria:hash-table-values
                                             *writer-functions*))))
                        ,@body))))
            (with-simple-restart (continue "disable material")
              (compile nil l)))
          (lambda (&rest r) (declare (ignore r)))))))
