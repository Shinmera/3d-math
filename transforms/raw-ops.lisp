#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.transforms)

(define-template t+ <t> (x a b)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type)))
      ;; we have to copy aloc here in case (EQ X A)
      (let ((aloc (vcopy ,(place-form type :location 'a))))
        (declare (dynamic-extent aloc))
        (!v* ,(place-form type :location 'x) ,(place-form type :scaling 'a) ,(place-form type :location 'b))
        (!q* ,(place-form type :location 'x) ,(place-form type :rotation 'a) ,(place-form type :location 'x))
        (!v+ ,(place-form type :location 'x) ,(place-form type :location 'x) aloc))
      (!v* ,(place-form type :scaling 'x) ,(place-form type :scaling 'a) ,(place-form type :scaling 'b))
      (!q* ,(place-form type :rotation 'x) ,(place-form type :rotation 'a) ,(place-form type :rotation 'b))
      x)))

(define-template t- <t> (x a b)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type)))
      ;; implicit tinv of b followed by t+
      x)))

(define-template t= <t> (a b)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) a b)
               (return-type boolean))
      (and (v= ,(place-form type :location 'a) ,(place-form type :location 'b))
           (v= ,(place-form type :scaling 'a) ,(place-form type :scaling 'b))
           (q= ,(place-form type :rotation 'a) ,(place-form type :rotation 'b))))))

(define-template t~= <t> (a b)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) a b)
               (return-type boolean))
      (and (v~= ,(place-form type :location 'a) ,(place-form type :location 'b))
           (v~= ,(place-form type :scaling 'a) ,(place-form type :scaling 'b))
           (q~= ,(place-form type :rotation 'a) ,(place-form type :rotation 'b))))))

(define-template t<- <t> (x a)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      (v<- ,(place-form type :location 'x) ,(place-form type :location 'a))
      (v<- ,(place-form type :scaling 'x) ,(place-form type :scaling 'a))
      (q<- ,(place-form type :rotation 'x) ,(place-form type :rotation 'a))
      x)))

(define-template t*v <t> (x a b)
  (let ((type (type-instance 'transform-type <t>))
        (vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type vtype) x b)
               (return-type ,(lisp-type type)))
      (!v* x b ,(place-form type :scaling 'a))
      (!q* x ,(place-form type :rotation 'a) x))))

(define-template t*p <t> (x a b)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type vtype) x b)
               (return-type ,(lisp-type type)))
      (!v* x b ,(place-form type :scaling 'a))
      (!q* x ,(place-form type :rotation 'a) x)
      (!v+ x ,(place-form type :location 'a) x))))

(define-template tinv <t> (x a)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      (!vinv (place-form type :scaling 'x) (place-form type :scaling 'a))
      (!qinv (place-form type :rotation 'x) (place-form type :rotation 'a))
      (!v* (place-form type :location 'x) (place-form type :location 'a) (place-form type :scaling 'x))
      (!v* (place-form type :location 'x) (place-form type :location 'x) (,<t> -1))
      (!q* (place-form type :location 'x) (place-form type :rotation 'x) (place-form type :location 'x))
      x)))

(define-template t*p-inv <t> (x a b)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      
      x)))

(define-template tmix <t> (x a b tt)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type)))
      
      x)))

(define-template tmat <s> <t> (x a)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) a)
               (return-type ,(lisp-type type)))
      
      x)))

(define-template tfrom-mat <s> <t> (x a)
  (let ((type (type-instance 'transform-type <t>)))
    `((declare (type ,(lisp-type type) x)
               (return-type ,(lisp-type type)))
      
      x)))

;; [ ] t+
;; [ ] t-
;; [ ] t=
;; [ ] t~=
;; [ ] t<-
;; [ ] t*t
;; [ ] t*v
;; [ ] t*p
;; [ ] tinv
;; [ ] t*p-inv
;; [ ] tmix
;; [ ] tmat
;; [ ] tfrom-mat
;; [ ] tdtransform
;; [ ] tfrom-dtransform
;; [ ] tmove
;; [ ] tmove-by
;; [ ] toffset
;; [ ] toffset-by
;; [ ] tscale
;; [ ] tscale-by
;; [ ] trotate
;; [ ] trotate-by
;; [ ] tx
;; [ ] ty
;; [ ] tz
