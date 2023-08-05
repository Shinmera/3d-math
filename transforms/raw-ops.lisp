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
      (let ((binv (,(lisp-type type))))
        (declare (dynamic-extent binv))
        (,(compose-name #\/ 'tinv <t>) binv b)
        (,(compose-name #\/ 't+ <t>) x a binv)))))

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
               (return-type ,(lisp-type vtype)))
      (!v* x b ,(place-form type :scaling 'a))
      (!q* x ,(place-form type :rotation 'a) x))))

(define-template t*p <t> (x a b)
  (let ((type (type-instance 'transform-type <t>))
        (vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type vtype) x b)
               (return-type ,(lisp-type vtype)))
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
  (let ((type (type-instance 'transform-type <t>))
        (vtype (type-instance 'vec-type 3 <t>))
        (qtype (type-instance 'quat-type <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type vtype) x b)
               (return-type ,(lisp-type vtype)))
      (let* ((invrot (,(lisp-type qtype)))
             (invscale (,(lisp-type vtype))))
        (declare (dynamic-extent invrot invscale))
        ;; Compute tinv inline
        (!vinv invscale (place-form type :scaling 'a))
        (!qinv invrot (place-form type :rotation 'a))
        (v<- x b)
        (!v* x x invscale)
        (!q* x invrot x)
        ;; Re-use the invscale here as a temp vector for the invloc
        (!v* invscale invscale (place-form type :location 'a))
        (!v* invscale invscale (,<t> -1))
        (!q* invscale invrot invscale)
        (!v+ x x invscale)))))

(define-template tmix <t> (x a b tt)
  (let ((type (type-instance 'transform-type <t>))
        (qtype (type-instance 'quat-type <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (type ,<t> tt)
               (return-type ,(lisp-type type)))
      (!vlerp (place-form type :location 'x) (place-form type :location 'a) (place-form type :location 'b) tt)
      (!vlerp (place-form type :scaling 'x) (place-form type :scaling 'a) (place-form type :scaling 'b) tt)
      (let ((rot (,(lisp-type qtype))))
        (declare (dynamic-extent rot))
        (if (< (q. (place-form type :rotation 'a) rot) 0)
            (!q- rot (place-form type :rotation 'b))
            (q<- rot (place-form type :rotation 'b)))
        (!qnlerp (place-form type :rotation 'x) (place-form type :rotation 'a) rot tt))
      x)))

(define-template tmat <t> (mx a)
  (let ((type (type-instance 'transform-type <t>))
        (mtype (type-instance 'mat-type 4 <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type mtype) mx)
               (return-type ,(lisp-type mtype)))
      (let* ((l ,(place-form type :location 'a))
             (s ,(place-form type :scaling 'a))
             (r ,(place-form type :rotation 'a))
             (x (qx r)) (y (qy r)) (z (qz r)) (w (qw r))
             (xx (* x x)) (xy (* x y)) (xz (* x z)) (xw (* x w))
             (yy (* y y)) (yz (* y z)) (yw (* y w))
             (zz (* z z)) (zw (* z w)))
        (msetf mx
               (* (vx s) (- 1 (* 2 (+ yy zz)))) (* (vy s) 2 (- xy zw)) (* (vz s) 2 (+ xz yw)) (vx l)
               (* (vx s) 2 (+ xy zw)) (* (vy s) (- 1 (* 2 (+ xx zz)))) (* (vz s) 2 (- yz xw)) (vy l)
               (* (vx s) 2 (- xz yw)) (* (vy s) 2 (+ yz xw)) (* (vz s) (- 1 (* 2 (+ xx yy)))) (vz l)
               0.0 0.0 0.0 1.0)))))

(define-template tfrom-mat <t> (x a)
  (let ((type (type-instance 'transform-type <t>))
        (qtype (type-instance 'quat-type <t>))
        (mtype (type-instance 'mat-type 4 <t>))
        (3mtype (type-instance 'mat-type 3 <t>)))
    `((declare (type ,(lisp-type type) x)
               (type ,(lisp-type mtype) a)
               (return-type ,(lisp-type type)))
      (!qfrom-mat ,(place-form type :rotation 'x) m)
      (let* ((m (,(lisp-type 3mtype)))
             (rmat (,(lisp-type 3mtype)))
             (rinv (,(lisp-type qtype))))
        (declare (dynamic-extent m rmat rinv))
        (!qinv rinv ,(place-form type :rotation 'x))
        (!qmat rmat rinv)
        (!mtransfer m a 3 3 0 0 0 0)
        (!m* m m rmat)
        (!mcol ,(place-form type :location 'x) m 3)
        (!mdiag ,(place-form type :scaling 'x) m))
      x)))

(do-type-combinations transform-type define-t+)
(do-type-combinations transform-type define-t-)
(do-type-combinations transform-type define-t=)
(do-type-combinations transform-type define-t~=)
(do-type-combinations transform-type define-t<-)
(do-type-combinations transform-type define-t*v)
(do-type-combinations transform-type define-t*p)
(do-type-combinations transform-type define-tinv)
(do-type-combinations transform-type define-t*p-inv)
(do-type-combinations transform-type define-tmix)
(do-type-combinations transform-type define-tmat)
(do-type-combinations transform-type define-tfrom-mat)

;; [x] t+
;; [x] t-
;; [x] t=
;; [x] t~=
;; [x] t<-
;; [x] t*v
;; [x] t*p
;; [x] tinv
;; [x] t*p-inv
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
