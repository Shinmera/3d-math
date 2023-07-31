#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.quaternions)

(define-template-type quat (<t>)
    (compose-name NIL (type-prefix <t>) 'quat)
  :include (vec-type 3 <t>)
  :print-object (lambda (name sv slots)
                  `(write (list ',name (qx ,name) (qy ,name) (qz ,name) (qw ,name))
                          :stream ,sv))
  (field (compose-name NIL (type-prefix <t>) 'qw)
         :type <t> :alias (list 3 'w :w)))

(define-template-type dual-quat (<t>)
    (compose-name NIL (type-prefix <t>) 'dual-quat)
  (field (compose-name NIL (type-prefix <t>) 'qreal)
         :type (compose-name NIL (type-prefix <t>) 'quat)
         :alias (list 0 'real :real))
  (field (compose-name NIL (type-prefix <t>) 'qdual)
         :type (compose-name NIL (type-prefix <t>) 'quat)
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
                           `(,,(place type i) quat))))
       (define-type-dispatch (setf ,name) (value quat)
         ,@(loop for type in instances
                 collect `((,(<t> type) ,(lisp-type type)) ,(<t> type)
                           (setf `(,,(place type i) quat) value)))))))

(define-quat-accessor qx 0)
(define-quat-accessor qy 1)
(define-quat-accessor qz 2)
(define-quat-accessor qw 3)

(define-type-allias *quat quat dquat)

(define-alias quat-p (thing)
  `(typep ,thing '*quat))
