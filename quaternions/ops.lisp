#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.quaternions)

(defmacro define-2quat-dispatch (op)
  `(define-templated-dispatch ,(compose-name NIL '!2q op) (x a b)
     ((quat-type 0 #(0 1)) squatop ,op <t>)
     ((quat-type 0 real) squatop ,op real)
     ((quat-type 0 0) 2quatop ,op)))

(defmacro define-1quat-dispatch (name op &rest template-args)
  `(define-templated-dispatch ,name (x a)
     ((quat-type 0) ,op ,@template-args)))

(defmacro define-quatcomp-dispatch (op &optional (comb 'and) (rop op))
  `(define-templated-dispatch ,(compose-name NIL '2q op) (a b)
     ((quat-type #(0 1)) squatreduce ,comb ,op <t>)
     ((quat-type real) squatreduce ,comb ,op real)
     ((real quat-type) (squatreduce ,comb ,rop real) b a)
     ((quat-type 0) 2quatreduce ,comb ,op)))

(define-templated-dispatch !2q* (x a b)
  ((quat-type 0 #(0 1)) squatop * <t>)
  ((quat-type 0 real) squatop * real)
  ((quat-type 0 0) q*q)
  ((#'(matching-vec 1) quat-type 0) q*v))

(define-2quat-dispatch +)
(define-2quat-dispatch -)
(define-2quat-dispatch /)
(define-2quat-dispatch min)
(define-2quat-dispatch max)

(define-quatcomp-dispatch =)
(define-quatcomp-dispatch ~=)
(define-quatcomp-dispatch /= or)
(define-quatcomp-dispatch < and >)
(define-quatcomp-dispatch <= and >=)
(define-quatcomp-dispatch > and <)
(define-quatcomp-dispatch >= and <=)

(define-1quat-dispatch q<- 1quatop identity)

(define-1quat-dispatch !1q- 1quatop -)
(define-1quat-dispatch !1q/ 1quatop /)
(define-1quat-dispatch !qabs 1quatop abs)
(define-1quat-dispatch !qunit qunit)
(define-1quat-dispatch !qunit* qunit*)

(define-type-reductor !q+ q<- !2q+)
(define-type-reductor !q* q<- !2q*)
(define-type-reductor !q- q<- !2q- !1q-)
(define-type-reductor !q/ q<- !2q/ !1q/)
(define-type-reductor !qmin q<- !2qmin)
(define-type-reductor !qmax q<- !2qmax)

(define-templated-dispatch !qrand (x)
  ((quat-type) random))
(define-templated-dispatch !qfrom-angle (x axis angle)
  ((quat-type #'(matching-vec 0) #(0 1)) qfrom-angle))
(define-templated-dispatch !qtowards (x from to)
  ((quat-type #'(matching-vec 0) 1) qtowards))
(define-templated-dispatch !qlookat (x dir up)
  ((quat-type #'(matching-vec 0) 1) qlookat))
(define-templated-dispatch !qexpt (x q exponent)
  ((quat-type 0 #(0 1)) qexpt))
(define-templated-dispatch !qmat (x q)
  ((mat3 quat) (qmat 3 f32) x q)
  ((mat4 quat) (qmat 4 f32) x q)
  ((dmat3 dquat) (qmat 3 f64) x q)
  ((dmat4 dquat) (qmat 4 f64) x q))
(define-templated-dispatch !qfrom-mat (x m)
  ((quat mat3) (qfrom-mat 3 f32) x m)
  ((quat mat4) (qfrom-mat 4 f32) x m)
  ((dquat dmat3) (qfrom-mat 3 f64) x m)
  ((dquat dmat4) (qfrom-mat 4 f64) x m))

(define-value-reductor q= 2q= and T)
(define-value-reductor q~= 2q~= and T)
(define-value-reductor q/= 2q/= and T)
(define-value-reductor q< 2q< and T)
(define-value-reductor q<= 2q<= and T)
(define-value-reductor q> 2q> and T)
(define-value-reductor q>= 2q>= and T)

(define-rest-alias q+ (q &rest others) qzero)
(define-rest-alias q- (q &rest others) qzero)
(define-rest-alias q* (q &rest others) qzero)
(define-rest-alias q/ (q &rest others) qzero)
(define-rest-alias qmin (q &rest others) qzero)
(define-rest-alias qmax (q &rest others) qzero)

(define-templated-dispatch q. (a b)
  ((quat-type 0) 2quatreduce * +))
(define-templated-dispatch qsqrlength (a)
  ((quat-type) 1quatreduce + sqr))
(define-templated-dispatch qangle (a)
  ((quat-type) qangle))
(define-templated-dispatch (setf qangle) (value a)
  ((#(1 1) quat-type) set-qangle))

(define-simple-alias qfrom-angle (axis angle) qzero)
(define-simple-alias qtowards (from to) qzero)
(define-simple-alias qlookat (direction up) qzero)
(define-simple-alias qexpt (q exponent) qzero)
(define-simple-alias qunit (q) qzero)
(define-simple-alias qunit* (q) qzero)

(define-alias qmat (q &optional (m (mat3)))
  `(!qmat ,m ,q))

(define-alias qfrom-mat (m)
  `(!qfrom-mat (quat) ,m))

(define-alias qaxis (q)
  `(vunit ,q))

(define-alias (setf qaxis) (value q)
  (let ((v (gensym "VALUE")))
    `(let ((,v ,value))
       (v<- ,q ,v)
       ,v)))

(define-alias qlength (q)
  `(sqrt (qsqrlength ,q)))

;; [ ] qsetf
;; [ ] nq+*
;; [ ] qconjugate
;; [ ] qinv
;; [ ] qmix
;; [ ] qnlerp
;; [ ] qslerp
