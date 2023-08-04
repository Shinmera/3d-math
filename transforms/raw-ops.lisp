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
               (type ,<t> tt)
               (return-type ,(lisp-type type)))
      (!vlerp (place-form type :location 'x) (place-form type :location 'a) (place-form type :location 'b) tt)
      (!vlerp (place-form type :scaling 'x) (place-form type :scaling 'a) (place-form type :scaling 'b) tt)
      (let ((rot (,(lisp-type type))))
        (declare (dynamic-extent rot))
        (if (< (q. (place-form type :rotation 'a) rot) 0)
            (!q- rot (place-form type :rotation 'b))
            (q<- rot (place-form type :rotation 'b)))
        (!qnlerp (place-form type :rotation 'x) (place-form type :rotation 'a) rot tt))
      x)))

(define-template tmat <t> (x a)
  (let ((type (type-instance 'transform-type <t>))
        (mtype (type-instance 'mat-type 4 <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type mtype) x)
               (return-type ,(lisp-type type)))
      (let* ((l (place-form type :location 'a))
             (s (place-form type :scaling 'a))
             (r (place-form type :rotation 'a))
             (x (qx r)) (y (qy r)) (z (qz r)) (w (qw r))
             (xx (* x x)) (xy (* x y)) (xz (* x z)) (xw (* x w))
             (yy (* y y)) (yz (* y z)) (yw (* y w))
             (zz (* z z)) (zw (* z w)))
        (msetf x
               (* (vx3 s) (- 1 (* 2 (+ yy zz)))) (* (vy3 s) 2 (- xy zw)) (* (vz3 s) 2 (+ xz yw)) (vx3 l)
               (* (vx3 s) 2 (+ xy zw)) (* (vy3 s) (- 1 (* 2 (+ xx zz)))) (* (vz3 s) 2 (- yz xw)) (vy3 l)
               (* (vx3 s) 2 (- xz yw)) (* (vy3 s) 2 (+ yz xw)) (* (vz3 s) (- 1 (* 2 (+ xx yy)))) (vz3 l)
               0.0 0.0 0.0 1.0)))))

(define-template tfrom-mat <s> <t> (x a)
  (let ((type (type-instance 'transform-type <t>))
        (mtype (type-instance 'mat-type <s> <t>))
        (3mtype (type-instance 'mat-type 3 <t>)))
    `((declare (type ,(lisp-type type) x)
               (return-type ,(lisp-type type)))
      (let* ((m (,(lisp-type 3mtype) a)))
        (declare (dynamic-extent m))
        ;; FIXME: this
        (!m* m m (qmat3 (qinv rot)))
        (!qfrom-mat (place-form type :rotation 'x) m)
        (!mcol (place-form type :location 'x) m 3)
        (!mdiag (place-form type :scaling 'x) m))
      x)))

;; [x] t+
;; [ ] t-
;; [x] t=
;; [x] t~=
;; [x] t<-
;; [x] t*v
;; [x] t*p
;; [x] tinv
;; [ ] t*p-inv
;; [x] tmix
;; [x] tmat
;; [x] tfrom-mat
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
