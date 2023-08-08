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

(define-templated-dispatch !2q2+ (x a b)
  ((quat2-type 0 0) per-part-2op !q+))

(define-templated-dispatch !2q2- (x a b)
  ((quat2-type 0 0) per-part-2op !q-))

(define-templated-dispatch !2q2* (x a b)
  :ignore-template-types (vec-type quat-type)
  ((quat2-type 0 0) q2*q2)
  ((#'(matching-vec 1) quat2-type 0) q2*v)
  ((#'(matching-quat 1) quat2-type 0) q2*q))

(define-templated-dispatch !2q2/ (x a b)
  ((quat2-type 0 0) per-part-2op !q/))

(define-templated-dispatch !1q2- (x a)
  ((quat2-type 0) per-part-1op !q-))

(define-templated-dispatch !1q2/ (x a)
  ((quat2-type 0) per-part-1op !q/))

(define-templated-dispatch 2q2= (a b)
  ((quat2-type 0) quat2-reduce and q=))

(define-templated-dispatch 2q2~= (a b)
  ((quat2-type 0) quat2-reduce and q~=))

(define-templated-dispatch 2q2/= (a b)
  ((quat2-type 0) quat2-reduce and q/=))

(define-templated-dispatch q2<- (x a)
  ((quat2-type 0) per-part-1op q<-))

(define-type-reductor !q2+ q2<- !2q2+)
(define-type-reductor !q2* q2<- !2q2*)
(define-type-reductor !q2- q2<- !2q2- !1q2-)
(define-type-reductor !q2/ q2<- !2q2/ !1q2/)

(define-templated-dispatch !q2conjugate (x a)
  ((quat2-type 0) per-part-1op !qconjugate))

(define-templated-dispatch !q2unit (x a)
  ((quat2-type 0) qunit))

(define-templated-dispatch !q2unit* (x a)
  ((quat2-type 0) qunit*))

(define-templated-dispatch !q2location (x a)
  :ignore-template-types (vec-type)
  ((#'(matching-vec 1) quat2-type) qlocation))

(define-templated-dispatch !q2from-location (x a b)
  :ignore-template-types (vec-type quat-type)
  ((quat2-type #'(matching-quat 0) #'(matching-vec 0)) qfrom-location))

(define-rest-alias q2+ (q &rest others) q2zero)
(define-rest-alias q2- (q &rest others) q2zero)
(define-rest-alias q2* (q &rest others) q2zero)
(define-rest-alias q2/ (q &rest others) q2zero)

(define-value-reductor q2= 2q2= and T)
(define-value-reductor q2~= 2q2~= and T)
(define-value-reductor q2/= 2q2/= and T)

(define-simple-alias q2conjugate (q) q2zero)
(define-simple-alias q2unit (q) q2zero)
(define-simple-alias q2unit* (q) q2zero)

(define-alias q2location (q)
  `(!q2location (quat) ,q))

(define-alias q2from-location (q v)
  `(!q2from-location (quat2) ,q ,v))

(define-alias q2. (a b)
  `(q. (q2real ,a) (q2real ,b)))

(define-alias q2sqrlength (a)
  `(qsqrlength (q2real ,a)))

(define-alias q2length (a)
  `(qlength (q2real ,a)))
