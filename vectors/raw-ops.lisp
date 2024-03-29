#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.vectors)

;; TODO: Avoid multiple array extraction by avoiding place-form

;; Element-Wise vector operation
(define-template 2vecop <op> <s> <t> (x a b)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type))
               (dynamic-extent a b)
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(,<op> ,(place-form type i 'a)
                                     ,(place-form type i 'b))))
      x)))

;; Element-wise scalar operation
(define-template svecop <op> <st> <s> <t> (x a s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (let ((s (,<t> s)))
        (psetf ,@(loop for i from 0 below <s>
                       collect (place-form type i 'x)
                       collect `(,<op> ,(place-form type i 'a) s))))
      x)))

;; Element-wise operation
(define-template 1vecop <op> <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(,<op> ,(place-form type i 'a))))
      x)))

(define-template 1svecop <op> <s> <t> (x s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x)
               (return-type ,(lisp-type type))
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(,<op> s)))
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
      (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
       (,<red> ,@(loop for i from 0 below <s>
                       collect `(,<comb> ,(place-form type i 'a)
                                         ,(place-form type i 'b))))))))

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
      (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
       (,<red> ,@(loop for i from 0 below <s>
                       collect `(,<comb> ,(place-form type i 'a))))))))

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
      (let ((s (,<t> s)))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
         (,<red> ,@(loop for i from 0 below <s>
                         collect `(,<comb> ,(place-form type i 'a) s))))))))

(define-template vec+* <st> <s> <t> (x a b s)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (type ,(if (eql <st> '<t>) <t> <st>) s)
               (return-type ,(lisp-type type))
               (dynamic-extent a b)
               inline)
      (let ((s (,<t> s)))
        (psetf ,@(loop for i from 0 below <s>
                       collect (place-form type i 'x)
                       collect `(+ ,(place-form type i 'a)
                                   (* ,(place-form type i 'b) s)))))
      x)))

(define-template inv <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(let ((v ,(place-form type i 'a)))
                                (if (<= (abs v) ,(case <t>
                                                   (f32 SINGLE-FLOAT-EPSILON)
                                                   (f64 DOUBLE-FLOAT-EPSILON)
                                                   (T 0)))
                                    (,<t> 0) (/ v)))))
      x)))

(define-template clamp <st> <s> <t> (x lower a upper)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a x)
               (type ,(case <st> (<t> <t>) (self (lisp-type type)) (T <st>)) lower upper)
               (return-type ,(lisp-type type))
               (dynamic-extent lower a upper))
      (let ,(if (eql <st> 'real)
                `((lower (,<t> lower))
                  (upper (,<t> upper))))
        (psetf ,@(loop for i from 0 below <s>
                       collect (place-form type i 'x)
                       collect `(clamp ,(if (eql <st> 'self) (place-form type i 'lower) 'lower)
                                       ,(place-form type i 'a)
                                       ,(if (eql <st> 'self) (place-form type i 'upper) 'upper)))))
      x)))

(define-template lerp <s> <t> (x from to tt)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x from to)
               (type single-float tt)
               (return-type ,(lisp-type type))
               (dynamic-extent from to))
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(,(case <t> ((f32 f64) <t>) (T 'floor))
                               (lerp ,(place-form type i 'from) ,(place-form type i 'to) tt))))
      x)))

(define-template random <s> <t> (x a var)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a var)
               (return-type ,(lisp-type type))
               (dynamic-extent a var))
      (flet ((random* (x var)
               (if (= 0.0 var)
                   x
                   (+ x (- (random var) (/ var 2f0))))))
        (psetf ,@(loop for i from 0 below <s>
                       collect (place-form type i 'x)
                       collect `(random* ,(place-form type i 'a) ,(place-form type i 'var)))))
      x)))

(define-template round <op> <s> <t> (x a divisor)
  (let ((type (type-instance 'vec-type <s> <t>))
        (op (ecase <op>
              (floor (case <t> ((f32 f64) 'ffloor) (T 'floor)))
              (round (case <t> ((f32 f64) 'fround) (T 'round)))
              (ceiling (case <t> ((f32 f64) 'fceiling) (T 'ceiling))))))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(,op ,(place-form type i 'a) divisor)))
      x)))

(define-template pnorm <s> <t> (a p)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,<t> p)
               (return-type ,(case <t> (f32 'f32) (f64 'f64) (T 'real)))
               (dynamic-extent a))
      (expt (+ ,@(loop for i from 0 below <s>
                       collect `(expt (abs ,(place-form type i 'a)) p)))
            (/ p)))))

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
      (psetf ,@(loop for name across (string <fields>)
                     for field = (intern (string name))
                     for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect (if (eql field '_)
                                 `(,<t> 0)
                                 (place-form type field 'a))))
      x)))

(define-template load <s> <t> (x a fields)
  (let ((type (type-instance 'vec-type <s> <t>)))
    (labels ((maybe-place (n)
               (if (<= <s> n)
                   `(error "Bad swizzle spec for vector with size ~a: ~a" ,<s> fields)
                   (place-form type n 'a)))
             (source (n)
               `(ecase (char fields ,n)
                  (#\X ,(maybe-place 0))
                  (#\Y ,(maybe-place 1))
                  (#\Z ,(maybe-place 2))
                  (#\W ,(maybe-place 3))
                  (#\_ (,(case n (0 'vx) (1 'vy) (2 'vz) (3 'vw)) x)))))
      `((declare (type ,(lisp-type type) a)
                 (type symbol fields)
                 (type *vec x)
                 (return-type *vec)
                 (dynamic-extent a))
        (let* ((fields (string fields))
               (len (length fields)))
          (when (< 4 len) (error "Bad swizzle spec: ~a" fields))
          (when (< 3 len) (setf (vw x) ,(source 3)))
          (when (< 2 len) (setf (vz x) ,(source 2)))
          (when (< 1 len) (setf (vy x) ,(source 1)))
          (setf (vx x) ,(source 0)))
        x))))

(define-template store <s> <t> (x a fields)
  (let ((type (type-instance 'vec-type <s> <t>)))
    (labels ((maybe-place (n value)
               (if (<= <s> n)
                   `(error "Bad swizzle spec for vector with size ~a: ~a" ,<s> fields)
                   `(setf ,(place-form type n 'x) ,value)))
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
        (let* ((fields (string fields))
               (len (length fields)))
          (when (< 4 len) (error "Bad swizzle spec: ~a" fields))
          (when (< 3 len) ,(source 3 '(vw a)))
          (when (< 2 len) ,(source 2 '(vz a)))
          (when (< 1 len) ,(source 1 '(vy a)))
          ,(source 0 '(vx a)))
        x))))

(define-template cross <s> <t> (x a b)
  (when (/= 3 <s>) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type))
               (dynamic-extent a b))
      (let ((ax ,(place-form type 0 'a))
            (ay ,(place-form type 1 'a))
            (az ,(place-form type 2 'a))
            (bx ,(place-form type 0 'b))
            (by ,(place-form type 1 'b))
            (bz ,(place-form type 2 'b)))
        (setf ,(place-form type 0 'x) (- (* ay bz) (* az by))
              ,(place-form type 1 'x) (- (* az bx) (* ax bz))
              ,(place-form type 2 'x) (- (* ax by) (* ay bx)))
        x))))

(define-template rotate <s> <t> (x a axis phi)
  (when (/= 3 <s>) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    (flet ((arith (qualifier)
             `(+ (* ,(place-form type qualifier 'a) cos)
                 (* ,(place-form type qualifier 'c) sin)
                 (* ,(place-form type qualifier 'axis) d (- 1 cos)))))
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
          (psetf ,(place-form type 0 'x) (,<t> ,(arith 0))
                 ,(place-form type 1 'x) (,<t> ,(arith 1))
                 ,(place-form type 2 'x) (,<t> ,(arith 2)))
          x)))))

(define-template rotate2 <s> <t> (x a phi)
  (when (/= 2 <s>) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type single-float phi)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let ((sin (sin phi))
            (cos (cos phi)))
        (psetf ,(place-form type 0 'x) (,<t> (- (* ,(place-form type 0 'a) cos) (* ,(place-form type 1 'a) sin)))
               ,(place-form type 1 'x) (,<t> (+ (* ,(place-form type 0 'a) sin) (* ,(place-form type 1 'a) cos))))
        x))))

(define-template cartesian <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let ((l ,(place-form type 0 'a))
            (p ,(place-form type 1 'a)))
        ,(case <s>
           (2 `(setf ,(place-form type 0 'x) (,<t> (* l (cos p)))
                     ,(place-form type 1 'x) (,<t> (* l (sin p)))))
           (3 `(let ((d ,(place-form type 2 'a)))
                 (setf ,(place-form type 0 'x) (,<t> (* l (cos p) (sin d)))
                       ,(place-form type 1 'x) (,<t> (* l (sin p) (sin d)))
                       ,(place-form type 2 'x) (,<t> (* l (cos d))))))
           (T (template-unfulfillable))))
      x)))

(define-template polar <s> <t> (x a)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type))
               (dynamic-extent a))
      (let ((len (,(compose-name #\/ '1vecreduce 'sqrt+ 'sqr <s> <t>) a))
            (atan (atan ,(place-form type 1 'a) ,(place-form type 0 'a))))
        ,(case <s>
           (2 `(setf ,(place-form type 0 'x) (,<t> len)
                     ,(place-form type 1 'x) (,<t> atan)))
           (3 `(let ((d ,(place-form type 2 'a)))
                 (setf ,(place-form type 0 'x) (,<t> len)
                       ,(place-form type 1 'x) (,<t> atan)
                       ,(place-form type 2 'x) (,<t> (/ len ,(place-form type 2 'a))))))
           (T (template-unfulfillable))))
      x)))

(define-template setf <s> <t> (a x y z w)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) a)
               (return-type ,(lisp-type type))
               (ignorable z w)
               inline)
      (let ((arr ,(place-form type :arr 'a)))
        (setf ,@(loop for i from 0 below <s>
                      for s in '(x y z w)
                      collect `(aref arr ,i)
                      collect `(,<t> ,s))))
      a)))

(define-template apply <s> <t> (x a f)
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) x a)
               (type (function (,<t>) T) f)
               (return-type ,(lisp-type type))
               (dynamic-extent a)
               inline)
      (psetf ,@(loop for i from 0 below <s>
                     collect (place-form type i 'x)
                     collect `(,<t> (funcall f ,(place-form type i 'a)))))
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

(do-type-combinations vec-type define-2vecop (+ - * / min max mod))
(do-type-combinations vec-type define-svecop (+ - * / min max mod grid) (<t> real))
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
(do-type-combinations vec-type define-round (floor round ceiling))
(do-type-combinations vec-type define-pnorm)
(do-type-combinations vec-type define-random)
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
