#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.vectors)

(defmacro define-2vec-dispatch (op)
  `(define-templated-dispatch ,(compose-name NIL '!2v op) (x a b)
     ((vec-type 0 #(0 1)) svecop ,op <t>)
     ((vec-type 0 real) svecop ,op real)
     ((vec-type 0 0) 2vecop ,op)))

(defmacro define-1vec-dispatch (name op &rest template-args)
  `(define-templated-dispatch ,name (x a)
     ((vec-type 0) ,op ,@template-args)))

(defmacro define-veccomp-dispatch (op &optional (comb 'and) (rop op))
  `(define-templated-dispatch ,(compose-name NIL '2v op) (a b)
     ((vec-type #(0 1)) svecreduce ,comb ,op <t>)
     ((vec-type real) svecreduce ,comb ,op real)
     ((real vec-type) (svecreduce ,comb ,rop real) b a)
     ((vec-type 0) 2vecreduce ,comb ,op)))

(define-2vec-dispatch +)
(define-2vec-dispatch -)
(define-2vec-dispatch *)
(define-2vec-dispatch /)
(define-2vec-dispatch min)
(define-2vec-dispatch max)
(define-2vec-dispatch mod)

(define-veccomp-dispatch =)
(define-veccomp-dispatch ~=)
(define-veccomp-dispatch /= or)
(define-veccomp-dispatch < and >)
(define-veccomp-dispatch <= and >=)
(define-veccomp-dispatch > and <)
(define-veccomp-dispatch >= and <=)

(define-templated-dispatch vsetf (a x y &optional z w)
  ((*vec4-type real real real real) setf)
  ((*vec3-type real real real) setf)
  ((*vec2-type real real) setf))

(define-templated-dispatch v<- (x a)
  ((vec-type 0) 1vecop identity)
  ((vec-type #(0 1)) 1svecop identity)
  ;; FIXME: this sucks!!
  #+3d-math-f32 ((vec2 real) (1svecop identity 2 f32) x (f32 a))
  #+3d-math-f32 ((vec3 real) (1svecop identity 3 f32) x (f32 a))
  #+3d-math-f32 ((vec4 real) (1svecop identity 4 f32) x (f32 a))
  #+3d-math-f64 ((dvec2 real) (1svecop identity 2 f64) x (f64 a))
  #+3d-math-f64 ((dvec3 real) (1svecop identity 3 f64) x (f64 a))
  #+3d-math-f64 ((dvec4 real) (1svecop identity 4 f64) x (f64 a))
  #+3d-math-i32 ((ivec2 real) (1svecop identity 2 i32) x (i32 a))
  #+3d-math-i32 ((ivec3 real) (1svecop identity 3 i32) x (i32 a))
  #+3d-math-i32 ((ivec4 real) (1svecop identity 4 i32) x (i32 a))
  #+3d-math-u32 ((uvec2 real) (1svecop identity 2 u32) x (u32 a))
  #+3d-math-u32 ((uvec3 real) (1svecop identity 3 u32) x (u32 a))
  #+3d-math-u32 ((uvec4 real) (1svecop identity 4 u32) x (u32 a)))

(define-templated-dispatch !valign (x a grid)
  ((vec-type 0 #(0 1)) svecop grid <t>)
  ((vec-type 0 real) svecop grid real))

(define-1vec-dispatch !1v- 1vecop -)
(define-1vec-dispatch !1v/ 1vecop /)
(define-1vec-dispatch !vabs 1vecop abs)

(define-type-reductor !v+ v<- !2v+)
(define-type-reductor !v* v<- !2v*)
(define-type-reductor !v- v<- !2v- !1v-)
(define-type-reductor !v/ v<- !2v/ !1v/)
(define-type-reductor !vmin v<- !2vmin)
(define-type-reductor !vmax v<- !2vmax)
(define-templated-dispatch !v+* (x a b s)
  ((vec-type 0 0 #(0 1)) vec+* <t>)
  ((vec-type 0 0 real) vec+* real))
(define-templated-dispatch !vinv (x a)
  ((vec-type 0) inv))
(define-templated-dispatch !vclamp (x low a up)
  ((vec-type #(0 1) 0 #(0 1)) clamp <t>)
  ((vec-type real 0 real) clamp real)
  ((vec-type 0 0 0) clamp self))
(define-templated-dispatch !vlerp (x from to tt)
  ((vec-type 0 0 single-float) lerp)
  ((vec-type 0 0 real) (lerp) x from to (float tt 0f0)))
(define-templated-dispatch !vfloor (x a &optional (divisor 1))
  ((vec-type 0 real) round floor))
(define-templated-dispatch !vround (x a &optional (divisor 1))
  ((vec-type 0 real) round round))
(define-templated-dispatch !vceiling (x a &optional (divisor 1))
  ((vec-type 0 real) round ceiling))
(define-templated-dispatch !vrand (x a var)
  ((vec-type 0 0) random)
  ;; FIXME: this sucks!
  ((vec-type real real) (random) x (nv+ (vzero x) a) (nv+ (vzero x) var))
  ((vec-type 0 real) (random) x a (nv+ (vzero x) var))
  ((vec-type real 0) (random) x (nv+ (vzero x) a) var))
(define-templated-dispatch !vload (x a fields)
  ((*vec vec-type symbol) load))
(define-templated-dispatch !vstore (x a fields)
  ((vec-type *vec symbol) store))
(define-templated-dispatch !vc (x a b)
  ((*vec3-type 0 0) cross))
(define-templated-dispatch !vrot (x a axis phi)
  ((*vec3-type 0 0 single-float) rotate))
(define-templated-dispatch !vrot2 (x a phi)
  ((*vec2-type 0 single-float) rotate2))
(define-templated-dispatch !vcartesian (x a)
  ((*vec2-type 0) cartesian)
  ((*vec3-type 0) cartesian))
(define-templated-dispatch !vpolar (x a)
  ((*vec2-type 0) polar)
  ((*vec3-type 0) polar))
(define-templated-dispatch !vapply (x a f)
  ((vec-type 0 function) apply)
  ((vec-type 0 symbol) (apply) x a (fdefinition f)))

(define-value-reductor v= 2v= and T)
(define-value-reductor v~= 2v~= and T)
(define-value-reductor v/= 2v/= and T)
(define-value-reductor v< 2v< and T)
(define-value-reductor v<= 2v<= and T)
(define-value-reductor v> 2v> and T)
(define-value-reductor v>= 2v>= and T)

(define-templated-dispatch v. (a b)
  ((vec-type 0) 2vecreduce + *))
(define-templated-dispatch vdistance (a b)
  ((vec-type 0) 2vecreduce sqrt+ sqr2))
(define-templated-dispatch vsqrdistance (a b)
  ((vec-type 0) 2vecreduce + sqr2))
(define-templated-dispatch v1norm (a)
  ((vec-type) 1vecreduce + abs))
(define-templated-dispatch vinorm (a)
  ((vec-type) 1vecreduce max abs))
(define-templated-dispatch v2norm (a)
  ((vec-type) 1vecreduce sqrt+ sqr))
(define-templated-dispatch vpnorm (a p)
  ((vec-type real) pnorm))
(define-templated-dispatch vsqrlength (a)
  ((vec-type) 1vecreduce + sqr))
(define-templated-dispatch vlike (a s)
  ((fvec integer) like f32)
  ((dvec integer) like f64)
  #+3d-math-u32 ((uvec integer) like u32)
  ((ivec integer) like i32))

(define-rest-alias v+ (v &rest others) vzero)
(define-rest-alias v- (v &rest others) vzero)
(define-rest-alias v* (v &rest others) vzero)
(define-rest-alias v/ (v &rest others) vzero)
(define-rest-alias vmin (v &rest others) vzero)
(define-rest-alias vmax (v &rest others) vzero)

(define-simple-alias vabs (v) vzero)
(define-simple-alias vmod (v modulus) vzero !2vmod)
(define-simple-alias vfloor (v &optional (d 1)) vzero)
(define-simple-alias vceiling (v &optional (d 1)) vzero)
(define-simple-alias vround (v &optional (d 1)) vzero)
(define-simple-alias vc (a b) vzero)
(define-simple-alias vrot (v axis phi) vzero)
(define-simple-alias vrot2 (v phi) vzero)
(define-simple-alias valign (v grid) vzero)
(define-simple-alias vcartesian (v) vzero)
(define-simple-alias vpolar (v) vzero)
(define-simple-alias vlerp (from to tt) vzero)
(define-simple-alias vapply (v func) vzero)

(define-alias vrand (&optional (v 0.0) (var 1.0))
  `(!vrand (typecase ,v (single-float (vec3)) (double-float (dvec3)) (integer (ivec3)) (T ,v)) ,v ,var))
(define-alias nvrand (v &optional (var 1))
  `(!vrand ,v ,v ,var))

(define-alias vorder (v fields)
  `(,'!vload (vlike ,v (length (string ,fields))) ,v ,fields))
(define-modifying-alias nvorder (v fields) !vload)

(define-alias (setf vorder) (source target fields)
  `(!vload ,target ,source ,fields))

(define-alias vunit (a)
  `(!v/ (vzero ,a) ,a (v2norm ,a)))
(define-alias nvunit (a)
  `(!v/ ,a ,a (v2norm ,a)))
(define-alias !vunit (x a)
  `(!v/ ,x ,a (v2norm ,a)))
(define-alias vunit* (a)
  `(let ((length (v2norm ,a)))
     (if (= 0 length) (vcopy ,a) (!v/ (vzero ,a) ,a length))))
(define-alias nvunit* (a)
  `(let ((length (v2norm ,a)))
     (if (= 0 length) ,a (!v/ ,a ,a length))))
(define-alias !vunit* (x a)
  `(let ((length (v2norm ,a)))
     (if (= 0 length) (v<- ,x ,a) (!v/ ,x ,a length))))
(define-alias vrotv (v by)
  `(let ((x (vzero ,v)))
     (!vrot x ,v #.(vec 1 0 0) (vx ,by))
     (!vrot x x #.(vec 0 1 0) (vy ,by))
     (!vrot x x #.(vec 0 0 1) (vz ,by))))
(define-alias nvrotv (v by)
  `(progn
     (!vrot ,v ,v #.(vec 1 0 0) (vx ,by))
     (!vrot ,v ,v #.(vec 0 1 0) (vy ,by))
     (!vrot ,v ,v #.(vec 0 0 1) (vz ,by))))
(define-alias vscale (a s)
  `(!2v* (vzero ,a) ,a (/ ,s (v2norm ,a))))
(define-alias nvscale (a s)
  `(!2v* ,a ,a (/ ,s (v2norm ,a))))
(define-alias vincf (a &optional (d 1))
  `(!2v+ ,a ,a ,d))
(define-alias vdecf (a &optional (d 1))
  `(!2v- ,a ,a ,d))
(define-alias vlength (a)
  `(v2norm ,a))
(define-alias v1+ (a)
  `(v+ ,a 1))
(define-alias v1- (a)
  `(v- ,a 1))
(define-alias vangle (a b)
  `(let ((a (/ (v. ,a ,b)
               (v2norm ,a)
               (v2norm ,b))))
     (acos (clamp -1.0 a +1.0))))
(define-alias vclamp (low x high)
  `(!vclamp (vzero ,x) ,low ,x ,high))
(define-alias nvclamp (low x high)
  `(!vclamp ,x ,low ,x ,high))
(define-simple-alias v+* (a b s) vzero)
(define-simple-alias vinv (a) vzero)

(define-alias vref (m i)
  `(aref (varr ,m) ,i))

(define-alias (setf vref) (value m i)
  ;; FIXME: coerce value!
  `(setf (aref (varr ,m) ,i) ,value))

(defmacro define-all-swizzlers (size)
  (labels ((permute (&rest lists)
             (cond ((cdr lists)
                    (let ((sub (apply #'permute (rest lists))))
                      (loop for item in (first lists)
                            append (loop for s in sub collect (list* item s)))))
                   (lists
                    (mapcar #'list (first lists)))
                   (T
                    NIL))))
    `(progn ,@(loop for comps in (apply #'permute (loop repeat size collect '(_ x y z w)))
                    for name = (apply #'compose-name NIL 'v comps)
                    collect `(progn
                               (export ',name)
                               (define-alias ,name (v)
                                 ,(format NIL "Extract a VEC~d made of [~{ ~a~} ]~%" (length comps) comps)
                                 `(vorder ,v ',',(apply #'compose-name NIL comps)))
                               (define-alias (setf ,name) (s v)
                                 ,(format NIL "Store into a VEC~d the fields [~{ ~a~} ]~%" (length comps) comps)
                                 `(!vstore ,v ,s ',',(apply #'compose-name NIL comps))))))))

(define-all-swizzlers 2)
(define-all-swizzlers 3)
(define-all-swizzlers 4)

(defmacro define-vector-constant (name x y &optional z w)
  (let ((z (when z (list z))) (w (when w (list w))))
    `(defconstant ,name (cond ((not (boundp ',name))
                               (vec ,x ,y ,@z ,@w))
                              ((v= (symbol-value ',name) (vec ,x ,y ,@z ,@w))
                               (symbol-value ',name))
                              (T (error "Attempting to redefine constant vector ~a with value ~a to ~a."
                                        ',name (symbol-value ',name) (vec ,x ,y ,@z ,@w)))))))
(define-vector-constant +vx2+ +1 +0)
(define-vector-constant +vy2+ +0 +1)

(define-vector-constant +vx3+ +1 +0 +0)
(define-vector-constant +vy3+ +0 +1 +0)
(define-vector-constant +vz3+ +0 +0 +1)

(define-vector-constant +vx4+ +1 +0 +0 +0)
(define-vector-constant +vy4+ +0 +1 +0 +0)
(define-vector-constant +vz4+ +0 +0 +1 +0)
(define-vector-constant +vw4+ +0 +0 +0 +1)

(define-vector-constant +vx+ +1 +0 +0)
(define-vector-constant +vy+ +0 +1 +0)
(define-vector-constant +vz+ +0 +0 +1)

(define-vector-constant -vx2+ -1 +0)
(define-vector-constant -vy2+ +0 -1)

(define-vector-constant -vx3+ -1 +0 +0)
(define-vector-constant -vy3+ +0 -1 +0)
(define-vector-constant -vz3+ +0 +0 -1)

(define-vector-constant -vx4+ -1 +0 +0 +0)
(define-vector-constant -vy4+ +0 -1 +0 +0)
(define-vector-constant -vz4+ +0 +0 -1 +0)
(define-vector-constant -vw4+ +0 +0 +0 -1)

(define-vector-constant -vx+ -1 +0 +0)
(define-vector-constant -vy+ +0 -1 +0)
(define-vector-constant -vz+ +0 +0 -1)
