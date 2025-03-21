#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.vectors)

;; Element-Wise vector operation
(define-template 2vecop <op> <s> <t> (x a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type))
               (dynamic-extent a b)
               inline)
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a))
            (b ,(place-form type 'arr 'b)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) (,<t> (,<op> (aref a i) (aref b i))))))
      x)))

;; Element-wise scalar operation
(define-template svecop <op> <st> <s> <t> (x a s)
  (let ((type (type-instance 'vec-type <s> <t>))
        (<st> (case <st> (<t> <t>) (T <st>))))
    `((declare (type ,(lisp-type type) x a)
               (type ,<st> s)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (let ((s (,(upgraded-type <t> <st>) s))
            (x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) (,<t> (,<op> (aref a i) s)))))
      x)))

;; Element-wise operation
(define-template 1vecop <op> <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) (,<op> (aref a i)))))
      x)))

(define-template 1svecop <op> <s> <t> (x s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x)
               (return-type ,(lisp-type type))
               inline)
      (let ((s (,<t> (,<op> s)))
            (x ,(place-form type 'arr 'x)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) s)))
      x)))

;; Element-wise vector reduce operation
(define-template 2vecreduce <red> <comb> rtype <s> <t> (a b)
  (let ((type (type-instance 'vec-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) a b)
               (return-type ,rtype)
               (dynamic-extent a b)
               inline)
      (let ((a ,(place-form type 'arr 'a))
            (b ,(place-form type 'arr 'b)))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
         (,<red> ,@(loop for i from 0 below <s>
                         collect `(,<comb> (aref a ,i) (aref b ,i)))))))))

;; Element-wise reduce operation
(define-template 1vecreduce <red> <comb> rtype <s> <t> (a)
  (let ((type (type-instance 'vec-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) a)
               (return-type ,rtype)
               (dynamic-extent a)
               inline)
      (let ((a ,(place-form type 'arr 'a)))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
         (,<red> ,@(loop for i from 0 below <s>
                         collect `(,<comb> (aref a ,i)))))))))

(define-template svecreduce <red> <comb> <st> rtype <s> <t> (a s)
  (let ((type (type-instance 'vec-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) a)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,rtype)
               (dynamic-extent a)
               inline)
      (let ((s (,<t> s))
            (a ,(place-form type 'arr 'a)))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
         (,<red> ,@(loop for i from 0 below <s>
                         collect `(,<comb> (aref a ,i) s))))))))

(define-template vec+* <st> <s> <t> (x a b s)
  (let ((type (type-instance 'vec-type <s> <t>))
        (<st> (if (eql <st> '<t>) <t> <st>)))
    `((declare (type ,(lisp-type type) x a b)
               (type ,<st> s)
               (return-type ,(lisp-type type))
               (dynamic-extent a b)
               inline)
      (let ((s (,(upgraded-type <t> <st>) s))
            (x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a))
            (b ,(place-form type 'arr 'b)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) (,<t> (+ (aref a i) (* (aref b i) s))))))
      x)))

(define-template inv <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        (do-times (i 0 ,<s>)
          (let ((v (aref a i)))
            (setf (aref x i)
                  (if (<= (abs v) ,(case <t>
                                     (f32 SINGLE-FLOAT-EPSILON)
                                     (f64 DOUBLE-FLOAT-EPSILON)
                                     (T 0)))
                      (,<t> 0) (/ v))))))
      x)))

(define-template clamp <st> <s> <t> (x lower a upper)
  (let* ((type (type-instance 'vec-type <s> <t>))
         (<st> (case <st> (<t> <t>) (self (lisp-type type)) (T <st>))))
    `((declare (type ,(lisp-type type) a x)
               (type ,<st> lower upper)
               (return-type ,(lisp-type type))
               (dynamic-extent lower a upper))
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        ,(if (equal <st> (lisp-type type))
             `(let ((lower ,(place-form type 'arr 'lower))
                    (upper ,(place-form type 'arr 'upper)))
                (do-times (i 0 ,<s>)
                  (setf (aref x i) (clamp (aref lower i) (aref a i) (aref upper i)))))
             `(let ((lower (,(upgraded-type <st> <t>) lower))
                    (upper (,(upgraded-type <st> <t>) upper)))
                (do-times (i 0 ,<s>)
                  (setf (aref x i) (,<t> (clamp lower (aref a i) upper)))))))
      x)))

(define-template lerp <s> <t> (x from to tt)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x from to)
               (type single-float tt)
               (return-type ,(lisp-type type))
               (dynamic-extent from to))
      (let ((x ,(place-form type 'arr 'x))
            (from ,(place-form type 'arr 'from))
            (to ,(place-form type 'arr 'to)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) (,<t> (lerp (aref from i) (aref to i) tt)))))
      x)))

(define-template round <op> <s> <t> (x a divisor)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        (do-times (i 0 ,<s>)
          (setf (aref x i) (,(ecase <op>
                               (floor (case <t> ((f32 f64) 'ffloor) (T 'floor)))
                               (round (case <t> ((f32 f64) 'fround) (T 'round)))
                               (ceiling (case <t> ((f32 f64) 'fceiling) (T 'ceiling)))
                               (truncate (case <t> ((f32 f64) 'ftruncate) (T 'truncate))))
                            (aref a i) divisor))))
      x)))

(define-template pnorm <s> <t> (a p)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,<t> p)
               (return-type ,(case <t> (f32 'f32) (f64 'f64) (T 'real)))
               (dynamic-extent a))
      (let ((a ,(place-form type 'arr 'a)))
        (expt (+ ,@(loop for i from 0 below <s>
                         collect `(expt (abs (aref a ,i)) p)))
              (/ p))))))

(define-template swizzle <fields> <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>))
        (target (type-instance 'vec-type (length (string <fields>)) <t>))
        (arity (loop for char across (string <fields>)
                     maximize (ecase (char-downcase char)
                                (#\_ 0) (#\x 1) (#\y 2) (#\z 3) (#\w 4)))))
    (when (< <s> arity)
      (template-unfulfillable))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type target) x)
               (return-type ,(lisp-type target))
               (dynamic-extent a))
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        (psetf ,@(loop for name across (string <fields>)
                       for field = (intern (string name))
                       for i from 0 below <s>
                       collect `(aref x ,i)
                       collect (if (eql field '_)
                                   `(,<t> 0)
                                   `(aref a ,i)))))
      x)))

(define-template load <s> <t> (x a fields)
  (let ((type (type-instance 'vec-type <s> <t>)))
    (labels ((maybe-place (n)
               (if (<= <s> n)
                   `(error "Bad swizzle spec for vector with size ~a: ~a" ,<s> fields)
                   `(aref a ,n)))
             (source (n)
               `(ecase (char fields ,n)
                  (#\X ,(maybe-place 0))
                  (#\Y ,(maybe-place 1))
                  (#\Z ,(maybe-place 2))
                  (#\W ,(maybe-place 3))
                  (#\_ (aref x ,n)))))
      `((declare (type ,(lisp-type type) a)
                 (type symbol fields)
                 (type *vec x)
                 (return-type *vec)
                 (dynamic-extent a))
        (let* ((x ,(place-form type 'arr 'x))
               (a ,(place-form type 'arr 'a))
               (fields (string fields))
               (len (length fields)))
          (when (< 4 len) (error "Bad swizzle spec: ~a" fields))
          (when (< 3 len) (setf (aref x 3) ,(source 3)))
          (when (< 2 len) (setf (aref x 2) ,(source 2)))
          (when (< 1 len) (setf (aref x 1) ,(source 1)))
          (setf (aref x 0) ,(source 0)))
        x))))

(define-template store <s> <t> (x a fields)
  (let ((type (type-instance 'vec-type <s> <t>)))
    (labels ((maybe-place (n value)
               (if (<= <s> n)
                   `(error "Bad swizzle spec for vector with size ~a: ~a" ,<s> fields)
                   `(setf (aref x ,n) ,value)))
             (source (n value)
               `(ecase (char fields ,n)
                  (#\X ,(maybe-place 0 value))
                  (#\Y ,(maybe-place 1 value))
                  (#\Z ,(maybe-place 2 value))
                  (#\W ,(maybe-place 3 value))
                  (#\_ ()))))
      `((declare (type ,(lisp-type type) x)
                 (type symbol fields)
                 (type *vec a)
                 (return-type ,(lisp-type type))
                 (dynamic-extent a))
        (let* ((a ,(place-form type 'arr 'a))
               (x ,(place-form type 'arr 'x))
               (fields (string fields))
               (len (length fields)))
          (when (< 4 len) (error "Bad swizzle spec: ~a" fields))
          (when (< 3 len) ,(source 3 '(aref a 3)))
          (when (< 2 len) ,(source 2 '(aref a 2)))
          (when (< 1 len) ,(source 1 '(aref a 1)))
          ,(source 0 '(aref a 0)))
        x))))

(define-template cross <s> <t> (x a b)
  (when (/= 3 <s>) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type))
               (dynamic-extent a b))
      (let* ((x ,(place-form type 'arr 'x))
             (a ,(place-form type 'arr 'a))
             (b ,(place-form type 'arr 'b))
             (ax (aref a 0))
             (ay (aref a 1))
             (az (aref a 2))
             (bx (aref b 0))
             (by (aref b 1))
             (bz (aref b 2)))
        (setf (aref x 0) (- (* ay bz) (* az by))
              (aref x 1) (- (* az bx) (* ax bz))
              (aref x 2) (- (* ax by) (* ay bx))))
      x)))

(define-template rotate <s> <t> (x a axis phi)
  (when (/= 3 <s>) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    (flet ((arith (qualifier)
             `(+ (* (aref a ,qualifier) cos)
                 (* (aref c ,qualifier) sin)
                 (* (aref axis ,qualifier) d (- 1 cos)))))
      `((declare (type ,(lisp-type type) x a axis)
                 (type single-float phi)
                 (return-type ,(lisp-type type))
                 (dynamic-extent a axis))
        (let* ((cos (the single-float (cos phi)))
               (sin (the single-float (sin phi)))
               (c (,(constructor type) ,(make-array <s> :element-type <t> :initial-element (funcall <t> 0))))
               (d (,(compose-name #\/ '2vecreduce '+ '* <s> <t>) axis a)))
          (declare (dynamic-extent c))
          (,(compose-name #\/ 'cross <s> <t>) c axis a)
          (let ((x ,(place-form type 'arr 'x))
                (a ,(place-form type 'arr 'a))
                (c ,(place-form type 'arr 'c))
                (axis ,(place-form type 'arr 'axis)))
            (psetf (aref x 0) (,<t> ,(arith 0))
                   (aref x 1) (,<t> ,(arith 1))
                   (aref x 2) (,<t> ,(arith 2)))))
        x))))

(define-template rotate2 <s> <t> (x a phi)
  (when (/= 2 <s>) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type single-float phi)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let ((sin (the single-float (sin phi)))
            (cos (the single-float (cos phi)))
            (x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        (psetf (aref x 0) (,<t> (- (* (aref a 0) cos) (* (aref a 1) sin)))
               (aref x 1) (,<t> (+ (* (aref a 0) sin) (* (aref a 1) cos)))))
      x)))

(define-template cartesian <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let* ((x ,(place-form type 'arr 'x))
             (a ,(place-form type 'arr 'a))
             (l (aref a 0))
             (p (aref a 1)))
        ,(case <s>
           (2 `(setf (aref x 0) (,<t> (* l (cos p)))
                     (aref x 1) (,<t> (* l (sin p)))))
           (3 `(let ((d (aref a 2)))
                 (setf (aref x 0) (,<t> (* l (cos p) (sin d)))
                       (aref x 1) (,<t> (* l (sin p) (sin d)))
                       (aref x 2) (,<t> (* l (cos d))))))
           (T (template-unfulfillable))))
      x)))

(define-template polar <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let* ((len (,(compose-name #\/ '1vecreduce 'sqrt+ 'sqr <s> <t>) a))
             (x ,(place-form type 'arr 'x))
             (a ,(place-form type 'arr 'a))
             (atan (atan (aref a 1) (aref a 0))))
        ,(case <s>
           (2 `(setf (aref x 0) (,<t> len)
                     (aref x 1) (,<t> atan)))
           (3 `(let ((d (aref a 2)))
                 (setf (aref x 0) (,<t> len)
                       (aref x 1) (,<t> atan)
                       (aref x 2) (,<t> (acos (/ d len))))))
           (T (template-unfulfillable))))
      x)))

(define-template setf <s> <t> (a x y z w)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (return-type ,(lisp-type type))
               (ignorable z w)
               inline)
      (let ((a ,(place-form type :arr 'a)))
        (setf ,@(loop for i from 0 below <s>
                      for s in '(x y z w)
                      collect `(aref a ,i)
                      collect `(,<t> ,s))))
      a)))

(define-template apply <s> <t> (x a f)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type (function (,<t>) T) f)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (let ((x ,(place-form type 'arr 'x))
            (a ,(place-form type 'arr 'a)))
        ;; NOTE: Have to use psetf here, X might be closed over in F.
        (psetf ,@(loop for i from 0 below <s>
                       collect `(aref x ,i)
                       collect `(,<t> (funcall f (aref a ,i))))))
      x)))

(define-template like <t> (a s)
  `((declare (return-type ,(ecase <t> (f32 'fvec) (f64 'dvec) (u32 'uvec) (i32 'ivec)))
             (dynamic-extent a)
             (ignore a)
             inline)
    (ecase s
      (2 (,(constructor (type-instance 'vec-type 2 <t>)) (make-array 2 :element-type ',<t> :initial-element (,<t> 0))))
      (3 (,(constructor (type-instance 'vec-type 3 <t>)) (make-array 3 :element-type ',<t> :initial-element (,<t> 0))))
      (4 (,(constructor (type-instance 'vec-type 4 <t>)) (make-array 4 :element-type ',<t> :initial-element (,<t> 0)))))))

(do-type-combinations vec-type define-2vecop (+ - * / min max mod random*))
(do-type-combinations vec-type define-svecop (+ - * / min max mod grid) (<t> real))
(do-type-combinations vec-type define-svecop (random*) (real))
(do-type-combinations vec-type define-1vecop (- / abs identity))
(do-type-combinations vec-type define-1svecop (identity))
(do-type-combinations vec-type define-2vecreduce (and) (= ~= /= < <= >= >) boolean)
(do-type-combinations vec-type define-svecreduce (and) (= ~= /= < <= >= >) (<t> real) boolean)
(do-type-combinations vec-type define-2vecreduce (or) (/=) boolean)
(do-type-combinations vec-type define-svecreduce (or) (/=) (<t> real) boolean)
(do-type-combinations vec-type define-2vecreduce (+) (*) <t>) ; dot
(do-type-combinations vec-type define-2vecreduce (sqrt+) (sqr2) float) ; dist
(do-type-combinations vec-type define-2vecreduce (+) (sqr2) <t>) ; sqrdist
(do-type-combinations vec-type define-1vecreduce (+ max) (abs) <t>) ; 1norm inorm
(do-type-combinations vec-type define-1vecreduce (sqrt+) (sqr) float) ; 2norm
(do-type-combinations vec-type define-1vecreduce (+) (sqr) <t>) ; sqrlen
(do-type-combinations vec-type define-vec+* (<t> real))
(do-type-combinations vec-type define-inv)
(do-type-combinations vec-type define-clamp (self <t> real))
(do-type-combinations vec-type define-lerp)
(do-type-combinations vec-type define-round (floor round ceiling truncate))
(do-type-combinations vec-type define-pnorm)
(do-type-combinations vec-type define-load)
(do-type-combinations vec-type define-store)
(do-type-combinations vec-type define-cross)
(do-type-combinations vec-type define-rotate)
(do-type-combinations vec-type define-rotate2)
(do-type-combinations vec-type define-cartesian)
(do-type-combinations vec-type define-polar)
(do-type-combinations vec-type define-setf)
(do-type-combinations vec-type define-apply)
#-3d-math-no-f32 (define-like f32)
#-3d-math-no-f64 (define-like f64)
#-3d-math-no-u32 (define-like u32)
#-3d-math-no-i32 (define-like i32)
