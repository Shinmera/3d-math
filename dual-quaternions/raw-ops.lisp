#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.dual-quaternions)

(define-template per-part-2op <op> <t> (x a b)
  (let ((type (type-instance 'quat2-type <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type)))
      (,<op> ,(place-form type :real 'x) ,(place-form type :real 'a) ,(place-form type :real 'b))
      (,<op> ,(place-form type :dual 'x) ,(place-form type :dual 'a) ,(place-form type :dual 'b))
      x)))

(define-template per-part-1op <op> <t> (x a)
  (let ((type (type-instance 'quat2-type <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      (,<op> ,(place-form type :real 'x) ,(place-form type :real 'a))
      (,<op> ,(place-form type :dual 'x) ,(place-form type :dual 'a))
      x)))

(define-template quat2-reduce <red> <comb> rtype <t> (x a)
  (let ((type (type-instance 'quat2-type <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,rtype))
      (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn)
       (,<red> (,<comb> ,(place-form type :real 'a) ,(place-form type :real 'b))
               (,<comb> ,(place-form type :dual 'a) ,(place-form type :dual 'b)))))))

(define-template q2*q2 <t> (x a b)
  (let ((type (type-instance 'quat2-type <t>))
        (qtype (type-instance 'quat-type <t>)))
    `((declare (type ,(lisp-type type) x a b)
               (return-type ,(lisp-type type)))
      (let ((ar (,(lisp-type qtype) ,(place-form type :real 'a)))
            (br (,(lisp-type qtype) ,(place-form type :real 'b)))
            (ad (,(lisp-type qtype) ,(place-form type :dual 'a)))
            (bd (,(lisp-type qtype) ,(place-form type :dual 'b))))
        (declare (dynamic-extent ar br ad bd))
        (let ((al (/ (qlength ar))))
          (!q* ar ar al) (!q* ad ad al))
        (let ((bl (/ (qlength br))))
          (!q* br br bl) (!q* bd bd bl))
        (!q* ,(place-form type :real 'x) ar br)
        (!q* ar ar bd)
        (!q* ad ad br)
        (!q+ ,(place-form type :dual 'x) ar ad))
      x)))

(define-template q2*v <t> (x a b)
  (let ((type (type-instance 'quat2-type <t>))
        (qtype (type-instance 'quat-type <t>))
        (vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type vtype) x b)
               (return-type ,(lisp-type vtype)))
      (let ((tmp (,(lisp-type qtype) ,(place-form type :real 'a))))
        (declare (dynamic-extent tmp))
        (!qconjugate tmp tmp)
        (!q* tmp tmp ,(place-form type :dual 'a) (,<t> 2))
        (!q* x ,(place-form type :real 'a) b)
        (!v+ x x tmp)))))

(define-template q2*q <t> (x a b)
  (let ((type (type-instance 'quat2-type <t>))
        (qtype (type-instance 'quat-type <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type qtype) x b)
               (return-type ,(lisp-type qtype)))
      (!q* x ,(place-form type :real 'a) b))))

(define-template qlocation <t> (x a)
  (let ((type (type-instance 'quat2-type <t>))
        (qtype (type-instance 'quat-type <t>))
        (vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type type) a)
               (type ,(lisp-type vtype) x)
               (return-type ,(lisp-type vtype)))
      (let ((tmp (,(lisp-type qtype) ,(place-form type :real 'a))))
        (declare (dynamic-extent tmp))
        (!qconjugate tmp tmp)
        (!q* tmp tmp ,(place-form type :dual 'a) (,<t> 2))
        (!v<- x tmp)))))

(define-template qfrom-location <t> (x q v)
  (let ((type (type-instance 'quat2-type <t>))
        (qtype (type-instance 'quat-type <t>))
        (vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type type) x)
               (type ,(lisp-type vtype) v)
               (type ,(lisp-type qtype) q)
               (return-type ,(lisp-type type)))
      (q<- ,(place-form type :real 'x) q)
      (v<- ,(place-form type :dual 'x) v)
      (setf (qw ,(place-form type :dual 'x)) 0.0)
      (!q* ,(place-form type :dual 'x) q ,(place-form type :dual 'x) (,<t> 0.5))
      x)))

(define-template qunit <t> (x a)
  (let ((type (type-instance 'quat2-type <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      (let ((length (/ (qlength ,(place-form type :real 'a)))))
        (!q* ,(place-form type :real 'x) ,(place-form type :real 'a) al) 
        (!q* ,(place-form type :dual 'x) ,(place-form type :dual 'a) al)
        x))))

(define-template qunit* <t> (x a)
  (let ((type (type-instance 'quat2-type <t>)))
    `((declare (type ,(lisp-type type) x a)
               (return-type ,(lisp-type type)))
      (let ((length (/ (qlength ,(place-form type :real 'a)))))
        (cond ((<= length ,(ecase <t>
                             (f32 SINGLE-FLOAT-EPSILON)
                             (f64 DOUBLE-FLOAT-EPSILON)))
               (qsetf ,(place-form type :real 'x) 0 0 0 1)
               (qsetf ,(place-form type :dual 'x) 0 0 0 0))
              (T
               (!q* ,(place-form type :real 'x) ,(place-form type :real 'a) al) 
               (!q* ,(place-form type :dual 'x) ,(place-form type :dual 'a) al)))
        x))))

(do-type-combinations quat2-type define-per-part-2op (!q+ !q- !q/))
(do-type-combinations quat2-type define-per-part-1op (!q- !q/ !qconjugate identity))
(do-type-combinations quat2-type define-quat2-reduce (and) (q= q~= q/=) boolean)
(do-type-combinations quat2-type define-q2*q2)
(do-type-combinations quat2-type define-q2*v)
(do-type-combinations quat2-type define-q2*q)
(do-type-combinations quat2-type define-qlocation)
(do-type-combinations quat2-type define-qfrom-location)
(do-type-combinations quat2-type define-qunit)
(do-type-combinations quat2-type define-qunit*)
