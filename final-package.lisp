#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.internal)

(export (loop for symbol being the symbols of '#:org.shirakumo.fraf.math
              collect symbol)
        '#:org.shirakumo.fraf.math)

(loop for symbol being the symbols of '#:org.shirakumo.fraf.math
      do (when (and (fboundp symbol) (documentation symbol 'function))
           (let ((! (find-symbol (format NIL "!~a" symbol) '#:org.shirakumo.fraf.math))
                 (n (find-symbol (format NIL "~a~a" :n symbol) '#:org.shirakumo.fraf.math)))
             (when (and ! (not (documentation ! 'function)))
               (setf (documentation ! 'function)
                     (format NIL "Transferring variant of ~a~%~%See ~a" symbol symbol)))
             (when (and n (not (documentation n 'function)))
               (setf (documentation n 'function)
                     (format NIL "Modifying variant of ~a~%~%See ~a" symbol symbol))))))

(defmacro define-*zero ()
  (flet ((expand (template &key (type #'lisp-type) (expansion (lambda (x) `(,(lisp-type x)))))
           (loop for instance in (instances template)
                 collect `((,(funcall type instance)) ,(lisp-type instance) ,(funcall expansion instance)))))
    `(define-type-dispatch *zero (x)
       ,@(expand 'vec-type
                 :type (lambda (x)
                         (or (ignore-errors
                              (when (eql (<s> x) 3)
                                `(and ,(lisp-type x) (not ,(lisp-type (type-instance 'quat-type (<t> x)))))))
                             (lisp-type x))))
       ,@(expand 'mat-type
                 :expansion (lambda (x)
                              (if (eql (<s> x) 'org.shirakumo.fraf.math.matrices::n)
                                  `(,(lisp-type x) ,(attribute x :rows 'x) ,(attribute x :cols 'x))
                                  `(,(lisp-type x)))))
       ,@(expand 'quat-type)
       ,@(expand 'quat2-type)
       ,@(expand 'transform-type))))

;; This package stuff sucks ass.
(in-package #:org.shirakumo.fraf.math)

(org.shirakumo.fraf.math.internal::define-*zero)

(org.shirakumo.type-templates:define-type-dispatch org.shirakumo.fraf.math.internal:*as (cl-user::x cl:type)
  (((cl:or fmat fvec quat quat2 transform) (cl:eql mat2)) mat2 (mat2))
  (((cl:or dmat dvec dquat dquat2 dtransform) (cl:eql mat2)) dmat2 (dmat2))
  (((cl:or fmat fvec quat quat2 transform) (cl:eql mat3)) mat3 (mat3))
  (((cl:or dmat dvec dquat dquat2 dtransform) (cl:eql mat3)) dmat3 (dmat3))
  (((cl:or fmat fvec quat quat2 transform) (cl:eql mat4)) mat4 (mat4))
  (((cl:or dmat dvec dquat dquat2 dtransform) (cl:eql mat4)) dmat4 (dmat4))
  (((cl:or fmat fvec quat quat2 transform) (cl:eql quat)) quat (quat))
  (((cl:or dmat dvec dquat dquat2 dtransform) (cl:eql quat)) dquat (dquat))
  (((cl:or fmat fvec quat quat2 transform) (cl:eql quat2)) quat2 (quat2))
  (((cl:or dmat dvec dquat dquat2 dtransform) (cl:eql quat2)) dquat2 (dquat2))
  (((cl:or fmat fvec quat quat2 transform) (cl:eql transform)) transform (transform))
  (((cl:or dmat dvec dquat dquat2 dtransform) (cl:eql transform)) dtransform (dtransform)))
