#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.quaternions)

(define-template-type quat (<t>)
    (compose-name NIL (type-prefix <t>) 'quat)
  :include (vec-type 3 <t>)
  (field (compose-name NIL '% (type-prefix <t>) 'qw)
         :type <t> :alias (list 3 'w :w 'r :r)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod compute-type-instance-definition ((type quat-type))
    `(progn
       ,(call-next-method)
       
       (defmethod print-object ((quat ,(lisp-type type)) stream)
         (write-constructor (list ',(lisp-type type) (qx quat) (qy quat) (qz quat) (qw quat)) stream))

       (defmethod describe-object ((quat ,(lisp-type type)) stream)
         (format stream "~&~s~%  [3d-math quaternion]~%~%" quat)
         ,@(loop for slot in (slots type)
                 when (or (not (realized-slot-p slot)) (find 'w (names slot)))
                 collect `(format stream "~a = ~@f~%" ',(accessor slot) (,(accessor slot) quat)))))))

(do-combinations define-quat
  (#-3d-math-no-f32 f32
   #-3d-math-no-f64 f64))

(define-slot-accessor quat-type qx 0)
(define-slot-accessor quat-type qy 1)
(define-slot-accessor quat-type qz 2)
(define-slot-accessor quat-type qw 3)
(define-slot-accessor quat-type qi 0)
(define-slot-accessor quat-type qj 1)
(define-slot-accessor quat-type qk 2)
(define-slot-accessor quat-type qr 3)

(define-type-alias *quat
  #-3d-math-no-f32 quat
  #-3d-math-no-f64 dquat)

(defmacro define-quat-constructors (<t>)
  (flet ((constructor (&rest args)
           `(,(constructor (type-instance 'quat-type <t>))
              (type-array 3 ,<t> ,@(butlast args))
              (,<t> ,(car (last args)))))
         (vtype (size)
           (lisp-type (type-instance 'vec-type size <t>))))
    (let ((*-name (compose-name NIL (type-prefix <t>) 'quat))
          (type (lisp-type (type-instance 'quat-type <t>))))
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

(do-type-combinations quat-type define-quat-constructors)

(macrolet ((emit ()
             `(define-type-dispatch qcopy (a)
                ,@(loop for instance in (instances 'quat-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (make-array 3 :element-type ',(<t> instance)
                                                 :initial-contents ,(place-form instance :arr 'a))
                                   ,(place-form instance :w 'a)))))))
  (emit))

(macrolet ((emit ()
             `(define-type-dispatch qzero (a)
                ,@(loop for instance in (instances 'quat-type)
                        for <t> = (<t> instance)
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (make-array 3 :element-type ',<t>
                                                 :initial-element (,<t> 0))
                                   (,<t> 0)))
                        collect `((,(lisp-type (type-instance 'vec-type 3 <t>))) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (make-array 3 :element-type ',<t>
                                                 :initial-element (,<t> 0))
                                   (,<t> 0)))))))
  (emit))
