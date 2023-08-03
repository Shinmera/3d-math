#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.quaternions)

(define-template-type quat (<t>)
    (compose-name NIL (type-prefix <t>) 'quat)
  :include (vec-type 3 <t>)
  (field (compose-name NIL '% (type-prefix <t>) 'qw)
         :type <t> :alias (list 3 'w :w)))

(defmethod compute-type-instance-definition ((type quat-type))
  `(progn
     ,(call-next-method)
     
     (defmethod print-object ((quat ,(lisp-type type)) stream)
       (write (list ',(lisp-type type) (qx quat) (qy quat) (qz quat) (qw quat))
              :stream stream))))

(define-template-type dual-quat (<t>)
    (compose-name NIL (type-prefix <t>) 'dual-quat)
  (field (compose-name NIL (type-prefix <t>) 'qreal)
         :type (lisp-type (type-instance 'quat <t>))
         :alias (list 0 'real :real))
  (field (compose-name NIL (type-prefix <t>) 'qdual)
         :type (lisp-type (type-instance 'quat <t>))
         :alias (list 1 'dual :dual)))

(defmacro do-quat-combinations (template &rest other-template-args)
  `(do-combinations ,template ,@other-template-args
     (#-3d-math-no-f32 f32
      #-3d-math-no-f64 f64)))

(do-quat-combinations define-quat)
(do-quat-combinations define-dual-quat)

(defmacro define-quat-accessor (name i)
  (let ((instances (instances 'quat-type)))
    `(progn
       (define-type-dispatch ,name (quat)
         ,@(loop for type in instances
                 collect `((,(lisp-type type)) ,(<t> type)
                           ,(place-form type i 'quat))))
       (define-type-dispatch (setf ,name) (value quat)
         ,@(loop for type in instances
                 collect `((,(<t> type) ,(lisp-type type)) ,(<t> type)
                           (setf ,(place-form type i 'quat) value)))))))

(define-quat-accessor qx 0)
(define-quat-accessor qy 1)
(define-quat-accessor qz 2)
(define-quat-accessor qw 3)
(define-quat-accessor qi 0)
(define-quat-accessor qj 1)
(define-quat-accessor qk 2)
(define-quat-accessor qr 3)

(define-type-alias *quat quat dquat)
(define-type-alias *dual-quat dual-quat ddual-quat)

(define-alias quat-p (thing)
  `(typep ,thing '*quat))

(define-alias dual-quat-p (thing)
  `(typep ,thing '*dual-quat))

(defmacro define-quat-constructors (<t>)
  (flet ((constructor (&rest args)
           `(,(constructor (type-instance 'quat-type 3 <t>))
              (type-array 3 ,<t> ,@(butlast args))
              (,<t> ,(car (last args)))))
         (vtype (size)
           (lisp-type (type-instance 'vec-type size <t>))))
    (let ((*-name (compose-name NIL (type-prefix <t>) 'quat))
          (type (lisp-type (type-instance 'quat-type 3 <t>))))
      `(progn
         (define-type-dispatch ,*-name (&optional a b c d)
           (() ,type
            ,(constructor 0 0 0 1))
           ((,(vtype 3)) ,type
            ,(constructor '(vx a) '(vy a) '(vz a) 1))
           ((,(vtype 3) real) ,type
            ,(constructor '(vx a) '(vy a) '(vz a) 'b))
           ((real real real) ,type
            ,(constructor 'a 'b 'c 1))
           ((real real real real) ,type
            ,(constructor 'a 'b 'c 'd))
           ((*quat) ,type
            ,(constructor '(qx a) '(qx a) '(qx a) '(qx a))))))))

#-3d-math-no-f32 (define-quat-constructors f32)
#-3d-math-no-f64 (define-quat-constructors f64)
