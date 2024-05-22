#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.internal)

;; We choose this limit in order to ensure that matrix indices
;; always remain within fixnum range. I'm quite certain you don't
;; want to use matrices as big as this allows anyway. You'll want
;; BLAS/LAPACK and/or someone much smarter than me for that.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *matrix-limit* (min (floor (sqrt array-dimension-limit))
                              (floor (sqrt most-positive-fixnum)))))

(deftype dimension ()
  '(integer 1 #.*matrix-limit*))

(deftype index ()
  '(integer 0 #.(1- *matrix-limit*)))

(defun attribute (type attribute mat-arg)
  (destructuring-bind (<s> <t>) (template-arguments type)
    (declare (ignore <t>))
    (ecase attribute
      (:dim-type (if (eql 'n <s>) 'dimension `(integer 0 ,(1- <s>))))
      (:idx-type (if (eql 'n <s>) 'dimension `(integer 0 ,(1- (* <s> <s>)))))
      (:cols (cond ((eql 'n <s>) `(,(place type 'cols) ,mat-arg))
                   ((typep type 'mat-type) <s>)
                   (T 1)))
      (:rows (cond ((eql 'n <s>) `(,(place type 'rows) ,mat-arg))
                   (T <s>)))
      (:len  (cond ((eql 'n <s>) `(length (,(place type 'arr) ,mat-arg)))
                   ((typep type 'mat-type) (* <s> <s>))
                   (T <s>))))))

(defun enlist (list-ish &rest els)
  (if (listp list-ish) list-ish (list* list-ish els)))

(define-type-with-converter f32 single-float (value)
  (float value 0f0))

(define-type-with-converter f64 double-float (value)
  (float value 0d0))

(define-type-with-converter u32 (unsigned-byte 32) (value)
  (ldb (byte 32 0) (truncate value)))

(define-type-with-converter i32 (signed-byte 32) (value)
  (let ((i (truncate value)))
    (if (<= 0 i)
        (ldb (byte 31 0) i)
        (- (ldb (byte 32 0) (- i))))))

(defmacro type-array (<s> <t> &rest values)
  (let ((array (gensym "ARRAY")))
    `(let ((,array (make-array ,<s> :element-type ',<t>)))
       ,@(loop for i from 0 below <s>
               for v in values
               collect `(setf (aref ,array ,i) (,<t> ,v)))
       ,array)))

(defun type-prefix (type)
  (ecase type
    (f32 '||)
    (f64 'd)
    (u32 'u)
    (i32 'i)))

(declaim (inline sqr sqr2 grid))
(defun sqr (a)
  (expt a 2))

(defun sqr2 (a b)
  (expt (- a b) 2))

(defun grid (a g)
  (* g (floor (+ a (/ g 2)) g)))

(defun sqrt+ (&rest a)
  (sqrt (apply #'+ a)))

(define-compiler-macro sqrt+ (&rest a)
  `(sqrt (+ ,@a)))

(declaim (inline lerp))
(defun lerp (from to n)
  (declare (optimize speed (safety 0)))
  (+ (* from (- 1 n)) (* to n)))

(declaim (inline clamp))
(defun clamp (min x max)
  (declare (optimize speed (safety 0)))
  (min (max x min) max))

(defun type-random (type low high)
  (ecase type
    (f32 (+ (f32 low) (random (- (f32 high) (f32 low)))))
    (f64 (+ (f64 low) (random (- (f64 high) (f64 low)))))
    (u32 (+ (u32 low) (random (- (u32 high) (u32 low)))))
    (i32 (+ (i32 low) (random (- (i32 high) (i32 low)))))))

(declaim (inline ~=))
(defun ~= (a b &optional (eps 1.0e-6))
  (<= (abs (- a b)) eps))

(declaim (inline ensure-function))
(defun ensure-function (functionish)
  (etypecase functionish
    (function functionish)
    (symbol (fdefinition functionish))))

(defmacro do-times (&environment env (var start end &optional (by 1) return) &body body)
  (if (and (constantp start env) (constantp end env) (constantp by env))
      `(block NIL
         ,@(loop for i from (eval start) below (eval end) by (eval by)
                 collect `(let ((,var ,i))
                            (declare (ignorable ,var))
                            ,@body))
         ,return)
      `(loop for ,var from ,start below ,end by ,by
             do (progn ,@body)
             finally (return ,return))))

(declaim (inline zero eye rand))
(defun zero (x y)
  (declare (ignore x y))
  0.0)

(defun eye (x y)
  (if (= x y) 1.0 0.0))

(defun rand (x y)
  (declare (ignore x y))
  (random 1.0))

(defmacro define-type-reductor (name transfer 2-op &optional 1-op)
  `(progn
     (defun ,name (target value &rest values)
       (cond ((null values)
              ,(if 1-op
                   `(,1-op target value)
                   `(,transfer target value)))
             ((null (cdr values))
              (,2-op target value (first values)))
             (T
              (,2-op target value (first values))
              (dolist (value (rest values) target)
                (,2-op target target value)))))

     (define-compiler-macro ,name (target value &rest values)
       (dbg "Expanding compiler macro (~a~{ ~a~})" ',name (list* value values))
       (cond ((null values)
              ,(if 1-op
                   ``(,',1-op ,target ,value)
                   ``(,',transfer ,target ,value)))
             ((null (cdr values))
              `(,',2-op ,target ,value ,(first values)))
             (T
              (let ((targetg (gensym "TARGET")))
                `(let ((,targetg ,target))
                   (,',2-op ,targetg ,value ,(first values))
                   ,@(loop for value in (rest values)
                           collect `(,',2-op ,targetg ,targetg ,value)))))))))

(defmacro define-value-reductor (name 2-op comb identity)
  `(progn
     (defun ,name (value &rest values)
       (cond ((null values)
              ,identity)
             ((null (cdr values))
              (,2-op value (first values)))
             (T
              (let* ((previous (first values))
                     (result (,2-op value previous)))
                (dolist (value (rest values) result)
                  (setf result (,comb result (,2-op previous value)))
                  (setf previous value))))))

     (define-compiler-macro ,name (value &rest values)
       (dbg "Expanding compiler macro (~a~{ ~a~})" ',name (list* value values))
       (cond ((null values)
              ,identity)
             ((null (cdr values))
              `(,',2-op ,value ,(first values)))
             (T
              (let ((previous (gensym "PREVIOUS"))
                    (next (gensym "NEXT")))
                `(let ((,previous ,value))
                   (,',comb ,@(loop for value in values
                                    collect `(let ((,next ,value))
                                               (prog1 (,',2-op ,previous ,next)
                                                 (setf ,previous ,next))))))))))))

(defmacro define-exports (&body names)
  `(export ',(loop for name in names
                   collect (intern (string name) *package*))
           *package*))

(defmacro define-pure-alias (name args zero &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     `(,',func (,',zero ,,(first args)) ,,@(lambda-list-variables args))))

(defmacro define-modifying-alias (name args &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     `(,',func ,,(first args) ,,@(lambda-list-variables args))))

(defmacro define-simple-alias (name args zero &optional (func (compose-name NIL '! name)))
  `(progn (define-pure-alias ,name ,args ,zero ,func)
          (define-modifying-alias ,(compose-name NIL 'n name) ,args ,func)))

(defmacro define-rest-alias (name args zero &optional (func (compose-name NIL '! name)))
  (let ((vars (lambda-list-variables args))
        (nname (compose-name NIL 'n name)))
    `(progn
       (defun ,name ,args
         (apply #',func (,zero ,(first args)) ,@vars))
       (defun ,nname ,args
         (apply #',func ,(first args) ,@vars))
       
       (define-compiler-macro ,name ,args
         `(let ,(list ,@(loop for var in (butlast vars)
                              collect `(list ',var ,var)))
            (,',func (,',zero ,',(first args)) ,',@(butlast vars) ,@,(car (last vars)))))
       (define-compiler-macro ,nname ,args
         `(let ,(list ,@(loop for var in (butlast vars)
                              collect `(list ',var ,var)))
            (,',func ,',(first args) ,',@(butlast vars) ,@,(car (last vars))))))))

(declaim (ftype (function (T) (values T &optional)) *like))
(defun *like (x) 0)

(declaim (ftype (function (T T) (values T &optional)) *as))
(defun *as (x type) 0)
