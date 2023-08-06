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
  ((quat2-type 0 0) per-part-2op !2+))

(define-templated-dispatch !2q2- (x a b)
  ((quat2-type 0 0) per-part-2op !2-))

(define-templated-dispatch !2q2* (x a b)
  :ignore-template-types (vec-type quat-type)
  ((quat2-type 0 0) q2*q2)
  ((#'(matching-vec 1) quat2-type 0) q2*v)
  ((#'(matching-quat 1) quat2-type 0) q2*q))

(define-templated-dispatch !2q2/ (x a b)
  ((quat2-type 0 0) per-part-2op !2/))

(define-templated-dispatch !1q2- (x a b)
  ((quat2-type 0 0) per-part-1op !2-))

(define-templated-dispatch !1q2/ (x a b)
  ((quat2-type 0 0) per-part-1op !2/))

(define-templated-dispatch q2<- (x a b)
  ((quat2-type 0 0) per-part-1op identity))

(define-type-reductor !q2+ q2<- !2q2+)
(define-type-reductor !q2* q2<- !2q2*)
(define-type-reductor !q2- q2<- !2q2- !1q2-)
(define-type-reductor !q2/ q2<- !2q2/ !1q2/)

(define-templated-dispatch !q2conjugate (x a b)
  ((quat2-type 0 0) per-part-1op !qconjugate))

(define-templated-dispatch !q2location (x a b)
  ((#'(matching-vec 1) quat2-type 0) qlocation))

(define-templated-dispatch !q2from-location (x a b)
  ((quat2-type #'(matching-quat 0) #'(matching-vec 0)) qfrom-location))

(define-rest-alias q2+ (q &rest others) q2zero)
(define-rest-alias q2- (q &rest others) q2zero)
(define-rest-alias q2* (q &rest others) q2zero)
(define-rest-alias q2/ (q &rest others) q2zero)

(define-value-reductor q2= 2q2= and T)
(define-value-reductor q2~= 2q2~= and T)
(define-value-reductor q2/= 2q2/= and T)

(define-simple-alias q2conjugate (q) q2zero)

(define-alias q2location (q)
  `(!qlocation (quat) ,q))

(define-alias q2from-location (q v)
  `(!q2from-location (quat2) ,q ,v))

(define-alias q2. (a b)
  `(q. (qreal ,a) (qreal ,b)))

(define-alias q2sqrlength (a)
  `(qsqrlength (qreal ,a)))

(define-alias q2length (a)
  `(qlength (qreal ,a)))

;; [*] q2+
;; [*] q2-
;; [*] q2*
;; [*] q2/
;; [*] q2=
;; [x] q2~=
;; [x] q2/=
;; [x] q2conjugate
;; [x] q2location
;; [x] q2from-location
;; [x] q2.
;; [x] q2sqrlength
;; [x] q2length
;; [ ] q2unit
;; [ ] q2unit*
