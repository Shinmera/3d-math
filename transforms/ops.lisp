#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.transforms)

(define-dependent-dispatch-type matching-vec (types i ref)
  (handler-case (apply #'type-instance 'vec-type 3 (template-arguments (nth ref types)))
    (error () NIL)))

(define-dependent-dispatch-type matching-mat (types i ref)
  (handler-case (apply #'type-instance 'mat-type 4 (template-arguments (nth ref types)))
    (error () NIL)))

(define-templated-dispatch !2t+ (x a b)
  ((transform-type 0 0) t+))

(define-templated-dispatch !2t- (x a b)
  ((transform-type 0 0) t-))

(define-templated-dispatch 2t= (a b)
  ((transform-type 0) t=))

(define-templated-dispatch 2t~= (a b)
  ((transform-type 0) t~=))

(define-templated-dispatch t<- (x a)
  ((transform-type 0) t<-))

(define-templated-dispatch !t*v (x a b)
  :ignore-template-types (vec-type)
  ((#'(matching-vec 1) transform-type 0) t*v))

(define-templated-dispatch !t*p (x a b)
  :ignore-template-types (vec-type)
  ((#'(matching-vec 1) transform-type 0) t*p))

(define-templated-dispatch !t*p-inv (x a b)
  :ignore-template-types (vec-type)
  ((#'(matching-vec 1) transform-type 0) t*p-inv))

(define-templated-dispatch !tinv (x a)
  ((transform-type 0) tinv))

(define-templated-dispatch !tmix (x a b tt)
  ((transform-type 0 0 #(0 0)) tmix))

(define-templated-dispatch !tmat (x a)
  :ignore-template-types (mat-type)
  ((#'(matching-mat 1) transform-type) tmat))

(define-templated-dispatch !tfrom-mat (x a)
  :ignore-template-types (mat-type)
  ((transform-type #'(matching-mat 0)) tfrom-mat))

(define-type-reductor !t+ t<- !2t+)
(define-type-reductor !t- t<- !2t- !tinv)

(define-value-reductor t= 2t= and T)
(define-value-reductor t~= 2t~= and T)
(define-rest-alias t+ (a &rest others) tzero)
(define-rest-alias t- (a &rest others) tzero)

(define-simple-alias t*v (a b) vzero)
(define-simple-alias t*p (a b) vzero)
(define-simple-alias t*p-inv (a b) vzero)

(define-simple-alias tinv (a) tzero)
(define-simple-alias tmix (a b tt) tzero)

(define-alias tmat (a &optional (m (mat4)))
  `(!tmat ,m ,a))

(define-alias tfrom-mat (m)
  `(!tfrom-mat (transform) ,m))

(define-alias tmove (a v)
  `(let ((tmp (vcopy ,v)))
     (declare (dynamic-extent tmp))
     (nv* tmp (tscaling ,a))
     (nq* (trotation ,a) tmp)
     (nv+ (tlocation ,a) tmp)
     ,a))

(define-alias tmove-by (a x y z)
  `(let ((tmp (vcopy (tlocation ,a))))
     (declare (dynamic-extent tmp))
     (vsetf tmp ,x ,y ,z)
     (tmove ,a tmp)
     ,a))

(define-alias toffset (a v)
  `(progn (nv+ (tlocation ,a) ,v)
          ,a))

(define-alias toffset-by (a x y z)
  `(let ((v (tlocation ,a)))
     (setf (vx v) (+ ,x (vx v))
           (vy v) (+ ,y (vy v))
           (vz v) (+ ,z (vz v)))
     ,a))

(define-alias tscale (a v)
  `(progn (nv+ (tscaling ,a) ,v)
          ,a))

(define-alias tscale-by (a x y z)
  `(let ((v (tscaling ,a)))
     (setf (vx v) (* ,x (vx v))
           (vy v) (* ,y (vy v))
           (vz v) (* ,z (vz v)))
     ,a))

(define-alias trotate (a q)
  `(progn (nq* (trotation ,a) ,q)
          ,a))

(define-alias trotate-by (a axis angle)
  `(let ((q (qcopy (trotation ,a))))
     (declare (dynamic-extent q))
     (!qfrom-angle q ,axis ,angle)
     (nq* (trotation ,a) q)
     ,a))

(define-alias tx (a) `(t*v a +vx3+))
(define-alias ty (a) `(t*v a +vy3+))
(define-alias tz (a) `(t*v a +vz3+))
