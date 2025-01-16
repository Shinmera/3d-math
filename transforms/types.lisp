#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.transforms)

(define-template-type transform (<t>)
    (compose-name NIL (type-prefix <t>) 'transform)
  (field (compose-name NIL '% (type-prefix <t>) 'location)
         :type (lisp-type (type-instance 'vec-type 3 <t>))
         :alias (list 0 'location :location))
  (field (compose-name NIL '% (type-prefix <t>) 'scaling)
         :type (lisp-type (type-instance 'vec-type 3 <t>))
         :alias (list 1 'scaling :scaling))
  (field (compose-name NIL '% (type-prefix <t>) 'rotation)
         :type (lisp-type (type-instance 'quat-type <t>))
         :alias (list 2 'rotation :rotation)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod compute-type-instance-definition ((type transform-type))
    `(progn
       ,(call-next-method)
       (defmethod print-object ((obj ,(lisp-type type)) stream)
         (let ((constructor (list ',(lisp-type type)
                                  ,@(loop for slot in (slots type)
                                          when (realized-slot-p slot)
                                          collect `(,(accessor slot) obj)))))
           (write-constructor constructor stream))))))

(do-combinations define-transform
  (#-3d-math-no-f32 f32
   #-3d-math-no-f64 f64))

(define-slot-accessor transform-type tlocation 0)
(define-slot-accessor transform-type tscaling 1)
(define-slot-accessor transform-type trotation 2)

(define-type-alias *transform
  #-3d-math-no-f32 transform
  #-3d-math-no-f64 dtransform)

(defmacro define-transform-constructors (<t>)
  (flet ((constructor (location scaling rotation)
           `(,(constructor (type-instance 'transform-type <t>))
              ,location
              ,scaling
              ,rotation)))
    (let ((*-name (compose-name NIL (type-prefix <t>) 'transform))
          (vtype (lisp-type (type-instance 'vec-type 3 <t>)))
          (qtype (lisp-type (type-instance 'quat-type <t>)))
          (type (lisp-type (type-instance 'transform-type <t>))))
      `(progn
         (define-type-dispatch ,*-name (&optional a b c)
           (() ,type
            ,(constructor `(,(lisp-type vtype)) `(,(lisp-type vtype) 1 1 1) `(,(lisp-type qtype))))
           ((,qtype) ,type
            ,(constructor `(,(lisp-type vtype)) `(,(lisp-type vtype) 1 1 1) 'a))
           ((,vtype) ,type
            ,(constructor 'a `(,(lisp-type vtype) 1 1 1) `(,(lisp-type qtype))))
           ((,vtype ,qtype) ,type
            ,(constructor 'a `(,(lisp-type vtype) 1 1 1) 'b))
           ((,vtype ,vtype) ,type
            ,(constructor 'a 'b `(,(lisp-type qtype))))
           ((,vtype ,vtype ,qtype) ,type
            ,(constructor 'a 'b 'c))
           ((,type) ,type
            ,(constructor '(vcopy (tlocation a)) '(vcopy (tscaling a)) '(qcopy (trotation a))))
           ((*transform) ,type
            ,(constructor `(,(lisp-type vtype) (tlocation a))
                          `(,(lisp-type vtype) (tscaling a))
                          `(,(lisp-type qtype) (trotation a)))))))))

(do-type-combinations transform-type define-transform-constructors)

(macrolet ((emit ()
             `(define-type-dispatch tcopy (a)
                ,@(loop for instance in (instances 'transform-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (vcopy ,(place-form instance 0 'a))
                                   (vcopy ,(place-form instance 1 'a))
                                   (qcopy ,(place-form instance 2 'a))))))))
  (emit))

(macrolet ((emit ()
             `(define-type-dispatch tzero (a)
                ,@(loop for instance in (instances 'transform-type)
                        for type = (first (template-arguments instance))
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (,(lisp-type (type-instance 'vec-type 3 type)) 0 0 0)
                                   (,(lisp-type (type-instance 'vec-type 3 type)) 1 1 1)
                                   (,(lisp-type (type-instance 'quat-type type)))))))))
  (emit))

(defun write-transform (transform stream)
  (etypecase transform
    (*transform
     (format stream "~
Location: ~7,2@f, ~7,2@f, ~7,2@f
Scaling:  ~7,2@f, ~7,2@f, ~7,2@f
Rotation: ~7,2@f, ~7,2@f, ~7,2@f, ~7,2@f"
             (vx (tlocation transform)) (vy (tlocation transform)) (vz (tlocation transform))
             (vx (tscaling transform)) (vy (tscaling transform)) (vz (tscaling transform))
             (qx (trotation transform)) (qy (trotation transform)) (qz (trotation transform)) (qw (trotation transform))))
    (*mat4
     (handler-case
         (let ((tf (tfrom-mat transform)))
           (write-transform tf stream))
       (error ()
         (write-matrix transform stream))))))
