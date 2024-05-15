#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.dual-quaternions)

(define-template-type quat2 (<t>)
    (compose-name NIL (type-prefix <t>) 'quat2)
  (field (compose-name NIL '% (type-prefix <t>) 'real)
         :type (lisp-type (type-instance 'quat-type <t>))
         :alias (list 0 'real :real))
  (field (compose-name NIL '% (type-prefix <t>) 'dual)
         :type (lisp-type (type-instance 'quat-type <t>))
         :alias (list 1 'dual :dual)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod compute-type-instance-definition ((type quat2-type))
    `(progn
       ,(call-next-method)
       (defmethod print-object ((obj ,(lisp-type type)) stream)
         (write (list ',(lisp-type type)
                      ,@(loop for slot in (slots type)
                              when (realized-slot-p slot)
                              collect `(,(accessor slot) obj)))
                :stream stream)))))

(do-combinations define-quat2
  (#-3d-math-no-f32 f32
   #-3d-math-no-f64 f64))

(define-slot-accessor quat2-type q2real 0)
(define-slot-accessor quat2-type q2dual 1)

(define-type-alias *quat2
  #-3d-math-no-f32 quat2
  #-3d-math-no-f64 dquat2)

(defmacro define-quat2-constructors (<t>)
  (flet ((constructor (real dual)
           `(,(constructor (type-instance 'quat2-type <t>)) ,real ,dual)))
    (let ((*-name (compose-name NIL (type-prefix <t>) 'quat2))
          (qtype (lisp-type (type-instance 'quat-type <t>)))
          (type (lisp-type (type-instance 'quat2-type <t>))))
      `(progn
         (define-type-dispatch ,*-name (&optional a b)
           (() ,type
            ,(constructor `(,(lisp-type qtype) 0 0 0 1) `(,(lisp-type qtype) 0 0 0 0)))
           ((,qtype) ,type
            ,(constructor 'a `(,(lisp-type qtype) 0 0 0 0)))
           ((,qtype ,qtype) ,type
            ,(constructor 'a 'b))
           ((,type) ,type
            ,(constructor '(qcopy (q2real a)) '(qcopy (q2dual a))))
           ((*quat2) ,type
            ,(constructor `(,(lisp-type qtype) (q2real a))
                          `(,(lisp-type qtype) (q2dual a)))))))))

(do-type-combinations quat2-type define-quat2-constructors)

(macrolet ((emit ()
             `(define-type-dispatch q2copy (a)
                ,@(loop for instance in (instances 'quat2-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (qcopy ,(place-form instance 0 'a))
                                   (qcopy ,(place-form instance 1 'a))))))))
  (emit))

(macrolet ((emit ()
             `(define-type-dispatch q2zero (a)
                ,@(loop for instance in (instances 'quat2-type)
                        for type = (first (template-arguments instance))
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (,(lisp-type (type-instance 'quat-type type)) 0 0 0 1)
                                   (,(lisp-type (type-instance 'quat-type type)) 0 0 0 0)))))))
  (emit))
