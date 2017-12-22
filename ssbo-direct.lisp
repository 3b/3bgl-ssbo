(in-package #:3bgl-ssbo)

;; experimental alternate API for streaming persistant mapped buffers

;;; split ssbo allocation from layouts.
;; ssbo part just acts as a ring buffer with some fixed
;; size. maintains GL state + start/end of free region, + GL
;; fence/sync for previously submitted ranges.  (or maybe just
;; double/triple buffer with fixed division instead of ring buffer?)

;; layout part contains packing info, + weak table of writer thunks
;; indexed by writer code.

;; user code defines writer functions with define-ssbo-writer, which
;; defines a function that takes ssbo layout object, pointer, size,
;; and arbitrary user parameters.

;; define-ssbo-writer defines a function that stores the input form in
;; a variable in the expansion, and uses that to index into the
;; function table in passed ssbo layout object. If not found, a writer
;; is generated for the layout and compiled and stored in the
;; table. Existing or new function is then called to write the data

;; (can't store redefinition info in function, since we might want to
;; have multiple layouts live at once, for example a shadow map shader
;; and normal render shader using same geometry)

;; when shaders are recompiled, resource manager loops through layouts
;; and repacks them if needed (only changing stored layout if new one
;; isn't EQUAL, so writer functions can use eql to check for changes)
;; and clears the hash of functions

;; define-ssbo-writer defines local macro for writing a slot/index,
;; which takes a name + value , and expands to call to code to write
;; value to appropriate location in buffer.
;; todo: add optional type checked fast-path to setter functions

(defclass persistent-buffer-range ()
  ((start :accessor start :initarg :start)
   (end :accessor end :initarg :end)
   (sync :accessor sync :initarg :sync)))

(defclass persistent-mapped-buffer ()
  ;; just a mapped buffer + size (possibly should remember creation
  ;; flags too?)
  ((buffer :reader buffer :initform nil :initarg :buffer)
   (size :reader size :initarg :size)
   (pointer :reader pointer :initarg :pointer)
   (ranges :reader ranges :initarg :ranges)
   (access :reader access :initarg :access)
   ;; index into RANGES of part currently being written
   (current-range :accessor current-range :initform 0)
   ;; offset into current range of beginning of unused space (will
   ;; probably write in multiple batches, like 1 write per material or
   ;; something, so keep track of how much we've written so far)
   (current-offset :accessor current-offset :initform 0)))

;; mostly intended for double/triple buffered streaming to GPU,
;; possibly should adjust API if other things start using it
(defun make-persistent-mapped-buffer (size &key (regions 3)
                                             (access :map-read))
  ;; currently allocates REGIONS buffers in SIZE * REGIONS byte
  ;; buffer, possibly should pass total size directly instead?
  (let* ((b (gl:create-buffer))
         #++(map (ecase access
                   (:read-only '(:map-read . #1=(:map-coherent :map-persistent
                                                 :dynamic-storage)))
                   (:write-only '(:map-write . #1#))
                   (:read-write '(:map-read :map-write . #1#))))
         (octets (* regions size))
         (r (make-array regions :initial-contents
                        (loop for i below regions
                              collect (make-instance 'persistent-buffer-range
                                                     :start (* size i)
                                                     :end (1- (* size (1+ i)))
                                                     :sync nil)))))
    (gl:named-buffer-storage b nil (list* access '(:map-coherent :map-persistent
                                                   :dynamic-storage))
                             :end octets)
    (make-instance 'persistent-mapped-buffer
                   :size octets
                   :access access
                   :buffer b
                   :ranges r
                   :pointer (%gl:map-named-buffer-range
                             b 0 octets
                             (list* access '(:map-coherent :map-persistent))))))

(defun reset-persistent-mapped-buffer (b)
  ;; fixme: do we need to wait on fences before deleting buffer?
  (loop for i across (ranges b)
        for s = (shiftf (sync i) nil)
        when s do (%gl:delete-sync s))
  (let ((buf (shiftf (slot-value b 'buffer) nil)))
    (setf (slot-value b 'pointer) nil)
    (when buf
      (%gl:unmap-named-buffer buf)
      (gl:delete-buffers (list buf)))))

(defmethod ensure-buffers ((b persistent-mapped-buffer))
  (unless (buffer b)
    (assert (plusp (size b)))
    (assert (not (pointer b)))
    (loop for r across (ranges b) do (assert (not (sync r))))
    (let ((bbuf (gl:create-buffer)))
      (gl:named-buffer-storage bbuf nil (list* (access b)
                                               '(:map-coherent :map-persistent
                                                 :dynamic-storage))
                               :end (size b))
      (setf (slot-value b 'buffer) bbuf)))
  (unless (pointer b)
    (assert (buffer b))
    (setf (slot-value b 'pointer)
          (%gl:map-named-buffer-range
           (buffer b) 0 (size b)
           (list :map-coherent :map-persistent (access b))))))

(defun current-region (b)
  (aref (ranges b) (current-range b)))

(defun use-region (b bytes)
  (let* ((r (current-region b))
         (start (current-offset b))
         (o (+ (start r) start)))
    (assert (< (+ o bytes) (end r)))
    (incf (current-offset b) bytes)
    start))

(defparameter *waits* 0)
(defmethod next-region ((b persistent-mapped-buffer))
  ;; if current region is unused, just return it
  (let ((cur (current-region b)))
    (when (and (zerop (current-offset b)) (not (sync cur)))
      (return-from next-region cur))
    ;; otherwise, make a fence
    (assert (not (sync cur)))
    (setf (sync cur) (%gl:fence-sync :sync-gpu-commands-complete 0))
    ;; switch to next region
    (setf (current-range b) (mod (1+ (current-range b)) (length (ranges b))))
    (setf (current-offset b) 0)
    (setf cur (current-region b))
    ;; and make sure region is available
    (when (sync cur)
      ;; not sure best ordering for clearing slot vs wait vs
      ;; delete. possibly should clear slot between wait/delete
      ;; instead?
      (let ((sync (shiftf (sync cur) nil)))
        ;; try once with no timeout
        (loop with w = (%gl:client-wait-sync sync nil 0)
              with delay = (* 1 1000000) ;; in ns
              with start = (get-internal-real-time)
              for waited from 0
              until (member w '(:already-signaled :condition-satisfied
                                :already-signaled-apple
                                :condition-satisfied-apple))
              when (= waited 10)
                ;; if we waited a few frames, complain
                do (cerror "Keep waiting" "waited ~s (~s) ms for sync on region ~s(~s) of buffer ~s? last result = ~s"
                           (floor (* waited delay) 1000000)
                           (* 1000.0
                              (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second))
                           (current-range b) cur b w)
                   ;; if not done yet, try again with a flush and wait
                   ;; for a bit (possibly should back off a bit after
                   ;; first?  any significant wait probably means
                   ;; something is wrong though)
              do (incf *waits*)
                 (setf w (%gl:client-wait-sync sync :sync-flush-commands
                                               delay)))
        (%gl:delete-sync sync)))
    cur))

(defclass ssbo-layout/static ()
  ((packing :accessor packing :initform nil :initarg :packing)
   ;; eql hash for speed, keyed by source forms from define-ssbo-writer macro
   (thunks :reader thunks
           ;; hopefully when a definition is recompiled, it's previous
           ;; source will no longer be reachable so this shouldn't
           ;; grow arbitrarily large
           :initform (trivial-garbage:make-weak-hash-table :weakness :key))))


(defclass writer-walker (cl-walker)
  ())

(defun members-to-hash-table (.slots)
  (if (typep .slots 'hash-table)
      .slots
      (let ((slots (make-hash-table)))
        ;; fixme: use a hash table in input instead of converting here
        (loop for s in .slots
              do (setf (gethash (getf s :name) slots) s))
        (setf (gethash '%%slot-list slots)
              (loop for s in .slots collect (getf s :name)))
        slots)))

(defun generate-value-writer (type pointer value &key index)
  (unless (gethash (gethash type *writer-structs* type)
                   *writer-functions*)
    (let* ((type (if (consp type)
                     type
                     (gethash type *writer-structs* type)))
           (ctype (gethash type *foreign-type-map*)))
      ;; fixme: gensyms as needed below
      (if ctype
          `((setf (cffi:mem-aref pointer ,ctype
                                 ,@(when index (list index)))
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
                      '(coerce value 'unsigned-byte)))))
          (ecase (car type)
            (:vec
             `((let ((value ,value)
                     (pointer ,pointer))
                 ,@(loop for i below (third type)
                         append (make-value-writer
                                 (second type)
                                 'pointer
                                 `(elt value ,i)
                                 :index i)))))
            (:mat
             ;; accept either a 4x4 or MxN matrix
             (let ((element-count (* (third type) (fourth type))))
               `((cond
                   ((= (array-total-size value)
                       ,element-count)
                    ;; fixme: handle matrix-stride
                    (loop with pointer = ,pointer
                          with value = ,value
                          for i below ,element-count
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
                    )))))
            (:array
             (error "shouldn't get here?"))
            (:struct
                (error "shouldn't get here?")))))))

(defvar *types*)
(defvar *pointer-var*)
(defvar *end*)

(defvar *context*)
(defvar *slots*)
(defvar *slots-written*)
(defvar *slots-index*)

(defvar *array-size*)
(defvar *array-stride*)
(defvar *array-count*)
(defvar *array-element-type*)

(defun generate-writer-context (*types* *pointer-var* *end* body)
  (funcall body))

(defmacro with-writer-context ((types pointer-var end) &body body)
  `(generate-writer-context ,types ',pointer-var ',end (lambda () ,@body)))

(defun generate-struct-context (type body)
  (let* ((struct-type (get-type type))
         (.slots (getf struct-type :members))
         (*context* type)
         (*slots* (members-to-hash-table .slots))
         ;; track which slots have writers in code. for
         ;; now assuming if there is code to write the
         ;; slot, it is always written (or any
         ;; conditional around the writer is valid so
         ;; default isn't needed)
         (*slots-written* (make-array (length .slots)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 0))
         (*slots-index* (let ((h (make-hash-table)))
                          (loop for s in .slots
                                for n = (getf s :name)
                                for i from 0
                                do (setf (gethash n h) i))
                          h)))
    (when .slots
      ;; if type has no members to write, don't need to generate any
      ;; code
      (flet ((set-defaults ()
               `(progn
                  ,@(loop for s in .slots
                          for w across *slots-written*
                          when (zerop w)
                            append (make-slot-writer
                                    s *pointer-var*
                                    (default-for-type s))))))
        (let ((def (set-defaults)))
          `(progn
            ,(set-defaults)
            ,(funcall body)))))))

(defun mark-slot-written (slot-name)
  (let* ((index (gethash slot-name *slots-index*)))
    (when index
      (setf (aref *slots-written* index) 1))))

(defun get-type (type)
  (cdr (gethash type *types*)))
(defun get-slot (slot)
  (gethash slot *slots*))

(defun generate-array-context (type body)
  (let* ((array-type (get-type type))
         (*array-size* (getf array-type :size))
         (*array-stride* (getf array-type :stride))
         (*context* type)
         ;; possibly should just get this from type name instead?
         (*array-count* (/ *array-size* *array-stride*))
         (*array-element-type* (second type)))

    (when (and (consp type) (eql (car type) :array)
               array-type)
      ;; no defaults for arrays for now, possibly should at least zero
      ;; it or something, but easier to just zero whole buffer from
      ;; caller than trying to detect nested arrays, or figure out how
      ;; many elements in unsized array
      (funcall body))))

#++
(defmethod walk-cons :before (car cdr (walker writer-walker))
  (format t "walk ~s~%" car))

(defmethod walk-cons (car cdr (walker writer-walker))
  (let* ((macro (macro-function car))
         (cmacro (unless macro
                   (compiler-macro-function car)))
         (form (list* car cdr))
         (environment nil))
    (cond
      (cmacro
       (let ((expanded (funcall cmacro form environment)))
         (if (eq expanded form)
             (if macro
                 (walk (funcall macro form environment) walker)
                 (call-next-method))
             (walk expanded walker))))
      (macro
       (walk (funcall macro form environment) walker))
      (t
       (call-next-method)))))

;; separate setters for slots and arrays, so we don't accidentally try
;; to use a slot name as name of a (probably nonexistent) variable
;; containing array index

(defwalker writer-walker (lambda lambda-list &rest body)
  `(lambda ,lambda-list ,@(walk-function-body walker lambda-list body)))

(defwalker writer-walker (set-slot slot-name value)
  (let ((slot (get-slot slot-name)))
    (unless slot (break "dd " slot-name *slots*))
    (when slot
      (mark-slot-written slot-name)
      (car (make-slot-writer slot *pointer-var* value)))))

(defwalker writer-walker (set-index index value)
  (let ((stride *array-stride*)
        ;; struct or arrays should be written with with-*@index
        (type *array-element-type*)
        (simple (not (gethash *array-element-type* *types*))))
    (when (and stride simple)
      (car (generate-value-writer type *pointer-var* value
                                  :index index)))))

(defwalker writer-walker (with-struct@slot slot &body body)
  (let* ((slot (get-slot slot))
         (pointer *pointer-var*)
         (offset (getf slot :offset)))
    (when slot
      `(let ((,pointer (cffi:inc-pointer ,pointer ,offset)))
         ,(generate-struct-context (car (getf slot :type))
                                   (lambda () (@ (cons 'progn body))))))))

(defwalker writer-walker (with-array@slot slot-name &body body)
  (let* ((slot (get-slot slot-name))
         (pointer *pointer-var*)
         (offset (getf slot :offset)))
    (when slot
      `(let ((,pointer (cffi:inc-pointer ,pointer ,offset)))
         ,(generate-array-context (car (getf slot :type))
                                  (lambda () (@ (cons 'progn body))))))))

(defwalker writer-walker (with-struct@index index &body body)
  (let ((stride *array-stride*)
        (pointer *pointer-var*)
        ;; base types should be written with set-index
        (type *array-element-type*)
        (struct (gethash *array-element-type* *types*)))

    (when (and stride struct)
      `(let ((,pointer (cffi:inc-pointer ,pointer (* ,stride ,index))))
         ,(generate-struct-context type
                                   (lambda () (@ `(progn ,@body))))))))

(defwalker writer-walker (with-array@index index &body body)
  (let ((stride *array-stride*)
        (pointer *pointer-var*)
        ;; base types should be written with set-index
        (type *array-element-type*)
        (struct (gethash *array-element-type* *types*)))
    (when (and stride struct)
      `(let ((,pointer (cffi:inc-pointer ,pointer (* ,stride ,index))))
         ,(generate-array-context type
                                  (lambda () (@ `(progn ,@body))))))))

(defun generate-writer-thunk (layout pointer-var size-var lambda-list body name)
  (let* ((*writer-defaults* (make-hash-table))
         (*writer-structs* (getf layout :types))
         (*writer-functions* (make-hash-table :test 'equalp))
         (*pointer-var* pointer-var)
         (type (getf layout :type))
         (types (getf layout :types))
         (*types* types)
         (w (make-instance 'writer-walker))
         (fun
           (generate-struct-context type
                                    (lambda ()
                                      (cons 'progn
                                            (loop for f in body
                                                  collect (walk f w)))))))
    (alexandria:with-gensyms (check-size base stride ok  fit-count)
      `(lambda ,lambda-list
         (block ,name
           (labels (,@ (remove nil
                         (mapcar 'second
                                 (alexandria:hash-table-values
                                  *writer-functions*)))
                       (check-size (count &key (errorp t))
                         (let* ((,base ,(getf layout :base))
                                (,stride ,(or (getf layout :stride)
                                              0))
                                (,check-size (+ ,base (* count ,stride)))
                                (,ok (< ,check-size ,size-var))
                                (,fit-count (min count
                                                 (floor (- ,size-var ,base)
                                                        ,stride))))
                           (when (and errorp (not ,ok))
                             (cerror "continue" "trying to write ~s bytes to ~s byte buffer"
                                     ,check-size ,size-var)
                             (return-from ,name nil))
                           ;; fixme: figure out which of these are
                           ;; actually useful. possibly easier to just
                           ;; return size, count, requested-size and
                           ;; let user check size to see if they can
                           ;; write at all and compare count to what
                           ;; they asked for to see if they are
                           ;; missing entries from end
                           (values ,check-size
                                   ,ok
                                   (if (< ,base ,size-var) t nil)
                                   ,fit-count
                                   (+ ,base (* ,stride ,fit-count))))))
             ,fun))))))

(defmacro define-ssbo-writer (&whole whole
                                name
                                (ssbo-layout-var pointer-var size-var
                                 &rest arguments)
                              &body body)
  (alexandria:with-gensyms (key fun)
    `(defun ,name (,ssbo-layout-var ,pointer-var ,size-var ,@arguments)
       (let* ((,key ',whole)
              (,fun (gethash ,key (thunks ,ssbo-layout-var))))
         (unless ,fun
           (setf ,fun (compile nil (generate-writer-thunk
                                    (packing ,ssbo-layout-var)
                                    ',pointer-var ',size-var
                                    `(,',pointer-var ,',size-var
                                                     ,@',arguments)
                                    ',body
                                    ',name)))
           (setf (gethash ,key (thunks ,ssbo-layout-var)) ,fun))
         (funcall ,fun ,pointer-var ,size-var ,@arguments)))))

(defun align (value alignment)
  (* alignment (ceiling value alignment)))

(defmacro with-current-region ((pointer
                                ;; make function names configurable
                                ;; for nested use
                                &key (remaining 'remaining)
                                  (use-bytes 'use-bytes)
                                  (bind-range 'bind-range)
                                  (bind 'bind))
                               ssbo &body body)
  (alexandria:with-gensyms (range offset start n bind-start)
    (alexandria:once-only (ssbo)
      `(let* ((,range (current-region ,ssbo))
              (,offset (current-offset ,ssbo))
              (,start (start ,range))
              (,bind-start (+ ,start ,offset))
              (,pointer (cffi:inc-pointer (pointer ,ssbo) ,bind-start)))
         ;; write data, call use-bytes one or more times to indicate
         ;; how much was written, then bind-range, send draw(s),
         ;; repeat write+use-bytes+bind-range as needed
         (flet ((,use-bytes (,n)
                  (incf (current-offset ,ssbo) ,n)
                  (cffi:incf-pointer ,pointer ,n)
                  (assert (< (current-offset ,ssbo)
                             (- (end ,range) (start ,range)))))
                (,remaining ()
                  (- (end ,range)
                      (+ ,start (current-offset ,ssbo))))
                (,bind-range (target index)
                  (%gl:bind-buffer-range target index (buffer ,ssbo)
                                         ,bind-start (- (current-offset ,ssbo)
                                                         ,offset))
                  (setf (current-offset ,ssbo)
                        ;; fixme: figure out correct value (needs to
                        ;; be at least alignment of struct, possibly
                        ;; more)
                        (align (current-offset ,ssbo) 256))
                  (setf ,offset (current-offset ,ssbo)
                        ,bind-start (+ ,start ,offset)))
                (,bind (target)
                  ;; can't bind some targets with bind-buffer-range
                  ;; so just bind whole thing and return an offset
                  (prog1 ,bind-start
                    (when target
                      ;; might not need to actually bind it if just
                      ;; attaching to a vao. (possibly should have
                      ;; better indicator than NIL for that?)
                      (%gl:bind-buffer target (buffer ,ssbo)))
                    (setf (current-offset ,ssbo)
                          ;; fixme: figure out correct value (needs to
                          ;; be at least alignment of struct, possibly
                          ;; more)
                          (align (current-offset ,ssbo) 256))
                    (setf ,offset (current-offset ,ssbo)
                          ,bind-start (+ ,start ,offset)))))
           ;; probably should use at least some of these, but
           ;; generated code might end up doing nothing

           (declare (ignorable #',use-bytes #',remaining #',bind #',bind-range))
           ,@body)))))

#++
(define-ssbo-writer write-materials (layout1 pointer1 size1 colors)
  (let ((s (check-size 5)))
    (set-slot 3BGL-AI-SHADERS::a 1)
    (with-array@slot 3bgl-ai-shaders::materials
      (loop for i below 5
            do (with-struct@index i
                 (set-slot 3bgl-ai-shaders::color
                           (print (aref colors (print i)))))))
    s))

