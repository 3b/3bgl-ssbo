(in-package #:3bgl-ssbo)

;; todo: probably should factor buffer stuff out from ssbo writer

(defmacro define-cffi-enum-compile-macro (function &rest lambda-list)
  `(define-compiler-macro ,function (&whole w &rest r)
       (labels ((expand1 (type value)
                  (when type
                    (format t "~&check ~s ~s: ~s~%"
                            type value
                            (cond
                              ((ignore-errors
                                (cffi:foreign-enum-value type value)))
                              ((ignore-errors
                                (cffi:foreign-bitfield-value type value)))
                              (t value))))
                  (cond
                    ((ignore-errors (cffi:foreign-enum-value type value)))
                    ((ignore-errors (cffi:foreign-bitfield-value type value)))
                    (t value)))
                (expand ()
                  (loop for (a . r1) on ',lambda-list
                        for (v . r2) on r
                        for ex = (expand1 a v)
                        collect ex
                        when (and r2 (not r1))
                          append r2)
                  ))
         (let ((expanded (expand)))
           (if (equalp expanded r)
               (format t "compiler macro ~s not expanded~%" ',function)
               (format t "compiler macro ~s expanded:~%  ~s~% -> ~s~%"
                       ',function w (cons (car w) expanded)))
           (if (equalp expanded r)
               w
               (cons (car w) expanded))))))


(defclass buffer ()
  ((name :reader name :accessor %name :initform nil)
   (size :reader size :accessor %size :initform 0)
   (flags :reader flags :accessor %flags :initform nil)))

;; todo: separate consstructor so it can have a compiler macro to expand enums
(defmethod initialize-instance :after ((o buffer) &key size data flags)
  (setf flags (if (or (symbolp flags) (consp flags))
                  (cffi:foreign-bitfield-value '%gl::mapbufferusagemask flags)
                  flags))
  (setf (%name o) (gl:create-buffer))
  (setf (%flags o) flags)
  (format t "~&allocated buffer ~s~%" (name o))
  (when data (assert size))
  (when size
    (format t "  -> buffer ~s size ~s flags ~s/~s~%" (name o)
            size  (flags o)
            (cffi:foreign-bitfield-symbols '%gl::mapbufferusagemask
                                           (flags o)))
    (gl:named-buffer-storage (name o) nil flags :end size)
    (setf (%size o) size)
    (etypecase data
      ;; fixme: handle CL vectors/arrays
      (cffi:foreign-pointer
       (%gl:named-buffer-sub-data (name o) 0 size data))
      (null ;; no data, do nothing
       nil))))

(defmethod bind-buffer-base (target index (o buffer))
  (%gl:bind-buffer-base target index (name o)))

(define-cffi-enum-compile-macro bind-buffer-base
  %gl:enum nil nil)

(defmethod resize ((o buffer) octets &key (copy-octets (size o))
                                       data-pointer data-size)
  (let ((b (gl:create-buffer)))
    (cond
      ;; option to fill buffer on creation for immutable data
      ((and data-pointer (not copy-octets)
            (or (not data-size) (= data-size octets)))
       (gl:named-buffer-storage b data-pointer (flags o) :end octets))
      (t ;; otherwise copy part and fill rest from specified pointer
       (gl:named-buffer-storage b (cffi:null-pointer) (flags o) :end octets)
       (when (and (%name o) copy-octets)
         (%gl:copy-named-buffer-sub-data (%name o) b 0 0
                                         (min copy-octets (size o))))
       (when data-pointer (assert data-size))
       (when data-pointer
         (let* ((offset (or copy-octets 0))
                (size (or data-size (- octets offset))))
           (assert (<= (+ offset size) octets))
           (%gl:named-buffer-sub-data (%name o)
                                      offset size
                                      data-pointer)))))
    (when (%name o)
      (gl:delete-buffers (list (shiftf (%name o) nil))))
    (setf (%name o) b
          (%size o) octets)
    octets))

(defmethod destroy ((o buffer))
  (when (name o)
    (gl:delete-buffers (list (shiftf (%name o) nil))))
    (setf (%size o) 0))

(defclass range ()
  ((start :reader start :initarg :start)
   (octet-count :reader octet-count :initarg :count)))

(defun range (&key start count end)
  ;; if we get both END and COUNT, complain if they don't match
  (when (and end count)
    (assert (= (+ start count) end)))
  (make-instance 'range :start start :count (or count (- end start))))

(defclass lock ()
  ((sync :reader sync :accessor %sync :initform nil)))

(defmethod initialize-instance :after ((o lock) &key)
  (setf (%sync o) (%gl:fence-sync :sync-gpu-commands-complete 0)))

(defmethod destroy ((o lock))
  (when (%sync o)
    (%gl:delete-sync (shiftf (%sync o) nil))))

(defclass locked-range (lock range)
  ())

(defclass locked-ranges ()
  ((ranges :accessor ranges :initform nil)))

(defmethod lock-range ((o locked-ranges) start octet-count)
  ;; we don't allow ranges with negative size, since that probably
  ;; means broken code
  (assert (not (minusp octet-count)))
  ;; but 0 size is just ignored since that could just be bad data
  ;; files
  (when (zerop octet-count)
    (return-from lock-range nil))
  (push (make-instance 'locked-range :start start :count octet-count)
        (ranges o)))

(defun ranges-intersect (range start octet-count)
  (and (plusp octet-count)
       (plusp (octet-count range))
       (let* ((s1 (start range))
              (c1 (octet-count range))
              (end1 (1- (+ s1 c1)))
              (end (1- (+ start octet-count))))
         (if (<= s1 start)
             (>= end1 start)
             (>= end s1)))))

#++
(let ((r (range :start 1 :end 10 :count 9)))
  (format t "~&~%")
  (loop for s in '(-1 0 1 2 5 9 10 11 16)
        sum (loop for e in '(-1 0 1 2 5 9 10 11 16)
                 for c = (- e s)
                 for r1 = (loop for i from (start r)
                                repeat (max 0 (octet-count r))
                                collect i)
                 for r2 = (loop for i from s repeat (max 0 c) collect i)
                 for in = (intersection r1 r2)
                 for ri = (unless (minusp c) (ranges-intersect r s c))
                  for good = (eql (not in) (not ri))
                  unless (minusp c)
                   do (format t "~s~%" (list good
                                             s c e ri in :r1 r1 :R2 r2))
                   and count (not good))
        finally (format t"~%~%")))

(defconstant +ns-per-ms+ 1000000)
(defparameter *waits* 0)

(defmethod wait-for-range ((o locked-range) start octet-count)
  ;; returns NIL if range isn't locked, returns T if wait succeeded,
  ;; errors if wait fails, or waits indefinitely if range is never
  ;; unlocked
  (unless (ranges-intersect o start octet-count)
    (return-from wait-for-range nil))
  (let ((sync (sync o)))
    ;; try once with no timeout
    (loop with w = (%gl:client-wait-sync sync nil 0)
          with delay = (* 1 +ns-per-ms+) ;; delay in ns
          with start = (get-internal-real-time)
          for waited from 0
          until (member w '(:already-signaled :condition-satisfied
                            :already-signaled-apple
                            :condition-satisfied-apple))
          ;; if we waited too long, complain
          when (= waited 10) ;; fixme: make this optional+configurable
            do (cerror "Keep waiting" "waited ~s (~s) ms for sync on region ~s(~s)? last result = ~s"
                       (floor (* waited delay) +ns-per-ms+)
                       (* 1000.0
                          (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second))
                       o (list (start o) (octet-count o)) w)
               ;; if not done yet, try again with a flush and wait
               ;; for a bit (possibly should back off a bit after
               ;; first?  any significant wait probably means
               ;; something is wrong though)
          do (incf *waits*)
             (setf w (%gl:client-wait-sync sync :sync-flush-commands
                                           delay)))
    t))

(defmethod wait-for-range ((o locked-ranges) start octet-count)
  ;; loop through ranges in O, if the overlap start+count,
  ;; %gl:wait-sync and remove from O
  (let ((count 0))
    (setf (ranges o)
          (loop for r in (ranges o)
                ;; todo: add restart to drop range on errors?
                when (wait-for-range r start octet-count)
                  do (destroy r)
                     (incf count)
                else
                  collect r))
    ;; less useful here than LOCKED-RANGE method, but return T if we
    ;; waited for anything
    (plusp count)))


(defclass buffer-with-locks (buffer locked-ranges)
  ())


