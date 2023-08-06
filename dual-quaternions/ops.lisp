#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.dual-quaternions)

(define-dependent-dispatch-type matching-vec (types i ref)
  (handler-case (apply #'type-instance 'vec-type 3 (template-arguments (nth ref types)))
    (error () NIL)))

(define-dependent-dispatch-type matching-mat (types i ref)
  (handler-case (apply #'type-instance 'mat-type 4 (template-arguments (nth ref types)))
    (error () NIL)))

(define-dependent-dispatch-type matching-quat (types i ref)
  (handler-case (apply #'type-instance 'quat-type (template-arguments (nth ref types)))
    (error () NIL)))
