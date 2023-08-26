#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.matrices)

(define-template setf <s> <t> (m args)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type list args)
               (return-type ,(lisp-type type))
               inline)
      (let ((marr ,(place-form type 'arr 'm)))
        (do-times (i 0 ,(attribute type :len 'm) 1 m)
          (let ((v (pop args)))
            (unless v (return))
            (setf (aref marr i) (,<t> v)))))
      m)))

(define-template mapply <s> <t> (x m f)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (type (function (,<t>) real) f)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len 'm) 1 x)
          (setf (aref xa i) (,<t> (funcall f (aref ma i)))))))))

(define-template mvec <s> <t> (m)
  (when (eql <s> 'n) (template-unfulfillable))
  (let ((type (type-instance 'vec-type <s> <t>)))
    `((declare (ignore m)
               (return-type ,(lisp-type type))
               inline)
      (,(lisp-type type)))))

(define-template copy <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,(lisp-type type)))
      (let ((orig ,(place-form type 'arr 'm))
            (arr (make-array ,(attribute type :len 'm) :element-type ',<t>)))
        (do-times (i 0 ,(attribute type :len 'm))
          (setf (aref arr i) (aref orig i)))
        (,(constructor type) arr ,@(when (eql 'n <s>) `(,(attribute type :rows 'm)
                                                        ,(attribute type :cols 'm))))))))

(define-template smatop <op> <st> <s> <t> (x m s)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x))
            (s (,<t> s)))
        (do-times (i 0 ,(attribute type :len 'm) 1 x)
          (setf (aref xa i) (,<op> (aref ma i) s)))))))

(define-template 2matop <op> <s> <t> (x m n)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m n)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (na ,(place-form type 'arr 'n))
            (xa ,(place-form type 'arr 'x)))
        ,@(when (eql <s> 'n)
            `((assert (and (= (mcols m) (mcols n)) (= (mrows m) (mrows n))) ()
                      "Matrices do not match in dimensions.")))
        (do-times (i 0 ,(attribute type :len 'm) 1 x)
          (setf (aref xa i) (,<op> (aref ma i) (aref na i))))))))

(define-template 1matop <op> <s> <t> (x m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len 'm) 1 x)
          (setf (aref xa i) (,<op> (aref ma i))))))))

(define-template 0matop <op> <s> <t> (x)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x)
               (return-type ,(lisp-type type))
               inline)
      (let ((xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len 'x) 1 x)
          (multiple-value-bind (y x) (floor i ,(attribute type :cols 'x))
            (setf (aref xa i) (,<t> (,<op> x y)))))))))

(define-template 2matreduce <red> <comb> rtype <s> <t> (m n)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) m n)
               (return-type ,rtype)
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (na ,(place-form type 'arr 'n))
             (r (,<comb> (aref ma 0) (aref na 0))))
        ,@(when (eql <s> 'n)
            `((assert (and (= (mcols m) (mcols n)) (= (mrows m) (mrows n))) ()
                      "Matrices do not match in dimensions.")))
        (do-times (i 1 ,(attribute type :len 'm) 1 r)
          (setf r (,<red> r (,<comb> (aref ma i) (aref na i)))))))))

(define-template 1matreduce <red> <comb> rtype <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) m)
               (return-type ,rtype)
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (r (,<comb> (aref ma 0))))
        (do-times (i 1 ,(attribute type :len 'm) 1 r)
          (setf r (,<red> r (,<comb> (aref ma i)))))))))

(define-template smatreduce <red> <comb> <st> rtype <s> <t> (m s)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) m)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,rtype)
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (s (,<t> s))
             (r (,<comb> (aref ma 0) s)))
        (do-times (i 1 ,(attribute type :len 'm))
          (setf r (,<red> r (,<comb> (aref ma i) s))))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn) r)))))

(define-template m*m <lhs> <rhs> (x l r)
  (let* ((lhs (type-instance <lhs>))
         (rhs (type-instance <rhs>))
         (<t> (<t> lhs))
         (ret (cond ((or (eql 'n (<s> lhs)) (eql 'n (<s> rhs)))
                     (type-instance 'mat-type 'n <t>))
                    ((eql (<s> lhs) (<s> rhs))
                     rhs)
                    ((typep rhs 'vec-type)
                     rhs)
                    (T
                     (template-unfulfillable)))))
    (unless (eq (<t> lhs) (<t> rhs))
      (template-unfulfillable))
    (when (and (typep lhs 'vec-type) (not (eql 'n (<s> rhs))))
      (template-unfulfillable))
    (when (and (typep rhs 'vec-type) (or (eql 'n (<s> lhs)) (not (<= (<s> rhs) (<s> lhs)))))
      (template-unfulfillable))
    (when (typep lhs 'vec-type)
      (setf ret (type-instance 'mat-type (<s> lhs) <t>)))
    `((declare (type ,(lisp-type lhs) l)
               (type ,(lisp-type rhs) r)
               (type ,(lisp-type ret) x)
               (return-type ,(lisp-type ret)))
      ,@(when (eql (<s> ret) 'n)
          `((assert (and (= ,(attribute lhs :cols 'l) ,(attribute rhs :rows 'r))) ()
                    "Matrices do not match in dimensions.")
            (assert (and (= ,(attribute ret :rows 'x) ,(attribute lhs :rows 'l))
                         (= ,(attribute ret :cols 'x) ,(attribute rhs :cols 'r))) ()
                         "Output matrix is not the right shape.")))
      (let* ((la ,(place-form lhs :arr 'l))
             (ra ,(place-form rhs :arr 'r))
             (xa ,(place-form ret :arr 'x))
             (ta (make-array ,(attribute ret :len 'x) :element-type ',<t>))
             (lc ,(attribute lhs :cols 'l))
             (rc ,(attribute rhs :cols 'r))
             (xi 0) (li 0))
        (declare (type dimension lc rc))
        (declare (type index xi li))
        (declare (dynamic-extent ta))
        ;; Perform matrix mul to temporary storage to avoid potential
        ;; thrashing in the case that LA or RA are eq to XA.
        (do-times (xy 0 ,(attribute ret :rows 'x) 1)
          (do-times (xx 0 ,(attribute ret :cols 'x) 1)
            (let ((c (,<t> 0))
                  (li li)
                  (ri xx))
              ;; KLUDGE: special handling for mat4/vec3
              ,(if (and (realp (<s> lhs)) (realp (<s> rhs)) (< (<s> rhs) (<s> lhs)))
                   `(progn
                      (do-times (i 0 ,(attribute rhs :rows 'r) 1)
                        (setf c (,<t> (+ c (* (aref la li) (aref ra ri)))))
                        (incf ri rc)
                        (incf li 1))
                      (do-times (i ,(attribute rhs :rows 'r) ,(attribute lhs :cols 'l) 1)
                        (setf c (,<t> (+ c (aref la li))))
                        (incf li 1)))
                   `(do-times (i 0 ,(attribute lhs :cols 'l) 1)
                      (setf c (,<t> (+ c (* (aref la li) (aref ra ri)))))
                      (incf ri rc)
                      (incf li 1)))
              (setf (aref ta xi) c)
              (incf xi)))
          (incf li lc))
        (do-times (i 0 ,(attribute ret :len 'x) 1 x)
          (setf (aref xa i) (aref ta i)))))))

(define-template mdet <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
      (let ((ma ,(place-form type 'arr 'm)))
        ,(case <s>
           ;; FIXME: This *sucks*. Can't we compute this expansion?
           (2 `(macrolet ((e (y x) `(aref ma (+ ,x (* ,y 2)))))
                 (- (* (e 0 0) (e 1 1))
                    (* (e 0 1) (e 1 0)))))
           (3 `(macrolet ((e (y x) `(aref ma (+ ,x (* ,y 3)))))
                 (- (+ (* (e 0 0) (e 1 1) (e 2 2))
                       (* (e 0 1) (e 1 2) (e 2 0))
                       (* (e 0 2) (e 1 0) (e 2 1)))
                    (+ (* (e 0 0) (e 1 2) (e 2 1))
                       (* (e 0 1) (e 1 0) (e 2 2))
                       (* (e 0 2) (e 1 1) (e 2 0))))))
           (4 `(macrolet ((e (y x) `(aref ma (+ ,x (* ,y 4)))))
                 (- (+ (* (e 0 3) (e 1 2) (e 2 1) (e 3 0)) (* (e 0 0) (e 1 1) (e 2 2) (e 3 3))
                       (* (e 0 1) (e 1 3) (e 2 2) (e 3 0)) (* (e 0 2) (e 1 1) (e 2 3) (e 3 0))
                       (* (e 0 2) (e 1 3) (e 2 0) (e 3 1)) (* (e 0 3) (e 1 0) (e 2 2) (e 3 1))
                       (* (e 0 0) (e 1 2) (e 2 3) (e 3 1)) (* (e 0 3) (e 1 1) (e 2 0) (e 3 2))
                       (* (e 0 0) (e 1 3) (e 2 1) (e 3 2)) (* (e 0 1) (e 1 0) (e 2 3) (e 3 2))
                       (* (e 0 1) (e 1 2) (e 2 0) (e 3 3)) (* (e 0 2) (e 1 0) (e 2 1) (e 3 3)))
                    (+ (* (e 0 2) (e 1 3) (e 2 1) (e 3 0)) (* (e 0 3) (e 1 1) (e 2 2) (e 3 0))
                       (* (e 0 1) (e 1 2) (e 2 3) (e 3 0)) (* (e 0 3) (e 1 2) (e 2 0) (e 3 1))
                       (* (e 0 0) (e 1 3) (e 2 2) (e 3 1)) (* (e 0 2) (e 1 0) (e 2 3) (e 3 1))
                       (* (e 0 1) (e 1 3) (e 2 0) (e 3 2)) (* (e 0 3) (e 1 0) (e 2 1) (e 3 2))
                       (* (e 0 0) (e 1 1) (e 2 3) (e 3 2)) (* (e 0 2) (e 1 1) (e 2 0) (e 3 3))
                       (* (e 0 0) (e 1 2) (e 2 1) (e 3 3)) (* (e 0 1) (e 1 0) (e 2 2) (e 3 3))))))
           (T
            `(multiple-value-bind (LU P s) (mlu m)
               (declare (ignore P))
               (let ((rows (mrows LU))
                     (cols (mcols LU))
                     (arr (marr LU)))
                 (loop for det = (,<t> 0) then (* (expt -1.0 s) det (aref arr (+ i (* i cols))))
                       for i from 0 below rows
                       finally (return det))))))))))

(define-template mtranspose <s> <t> (x m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (return-type ,(lisp-type type)))
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x))
            (mc ,(attribute type :cols 'm))
            (xc ,(attribute type :cols 'x)))
        ,@(when (eql <s> 'n)
            `((assert (and (= (mcols m) (mrows x)) (= (mrows m) (mcols x))))))
        (if (eq ma xa)
            (do-times (y 0 ,(attribute type :rows 'm) 1 x)
              (do-times (x y ,(attribute type :cols 'm) 1)
                (rotatef (aref xa (+ y (* x xc))) (aref xa (+ x (* y xc))))))
            (do-times (y 0 ,(attribute type :rows 'm) 1 x)
              (do-times (x 0 ,(attribute type :cols 'm) 1)
                (setf (aref xa (+ y (* x xc))) (aref ma (+ x (* y mc)))))))))))

(define-template mtrace <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
      (let ((ma ,(place-form type 'arr 'm))
            (r (,<t> 0)))
        (do-times (i 0 (expt 2 (min ,(attribute type :cols 'm) ,(attribute type :rows 'm))) (1+ ,(attribute type :cols 'm)) r)
          (setf r (+ r (aref ma i))))))))

(define-template mminor <s> <t> (m y x)
  (let ((type (type-instance 'mat-type <s> <t>))
        (itype (type-instance 'mat-type (case <s> (3 2) (4 3) (T 'n)) <t>)))
    `((declare (type ,(lisp-type type) m)
               (type index y x)
               (return-type ,<t>))
      (let* ((c ,(attribute type :cols 'm))
             (r ,(attribute type :rows 'm))
             (s ,(case (first (template-arguments itype))
                   (n `(,(lisp-type itype) (1- c) (1- r)))
                   (T `(,(lisp-type itype)))))
             (sa (marr s))
             (ma ,(place-form type 'arr 'm)))
        (declare (dynamic-extent s))
        (macrolet ((s (y x) `(aref sa (+ ,x (* ,y (1- c)))))
                   (m (y x) `(aref ma (+ ,x (* ,y c)))))
          ;; Copy the four subregions
          (do-times (i 0 y)
            (do-times (j 0 x)
              (setf (s i j) (m i j)))
            (do-times (j (1+ x) c)
              (setf (s i (1- j)) (m i j))))
          (do-times (i (1+ y) r)
            (do-times (j 0 x)
              (setf (s (1- i) j) (m i j)))
            (do-times (j (1+ x) c)
              (setf (s (1- i) (1- j)) (m i j))))
          (,(compose-name #\/ 'mdet (first (template-arguments itype)) <t>) s))))))

(define-template mcof <s> <t> (r m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m r)
               (return-type ,(lisp-type type)))
      (let* ((ra ,(place-form type 'arr 'r))
             (i 0))
        (do-times (y 0 ,(attribute type :rows 'm) 1 r)
          (do-times (x 0 ,(attribute type :cols 'm) 1)
            (setf (aref ra i) (* (,(compose-name #\/ 'mminor <s> <t>) m y x)
                                 (if (evenp (+ y x)) +1 -1)))
            (incf i)))))))

(define-template minv-affine <s> <t> (x m)
  (unless (eql 4 <s>) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m x)
               (return-type ,(lisp-type type)))
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x)))
        (macrolet ((e (y x) `(aref ma (+ ,x (* ,y 4))))
                   (f (y x) `(aref xa (+ ,x (* ,y 4)))))
          ;; Transpose the 3x3 rotation matrix
          ,@(loop for x from 0 below 3
                  append (loop for y from 0 below 3
                               collect `(setf (f ,y ,x) (e ,x ,y))))
          ;; Transpose the translation vector
          (let ((x (- (e 0 3)))
                (y (- (e 1 3)))
                (z (- (e 2 3))))
            (setf (f 0 3) (,<t> (+ (* x (e 0 0)) (* y (e 0 1)) (* z (e 0 2)))))
            (setf (f 1 3) (,<t> (+ (* x (e 1 0)) (* y (e 1 1)) (* z (e 1 2)))))
            (setf (f 2 3) (,<t> (+ (* x (e 2 0)) (* y (e 2 1)) (* z (e 2 2))))))
          ;; Fill the last row
          (setf (f 3 0) (,<t> 0))
          (setf (f 3 1) (,<t> 0))
          (setf (f 3 2) (,<t> 0))
          (setf (f 3 3) (,<t> 1))
          x)))))

(define-template minv <s> <t> (x m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m x)
               (return-type ,(lisp-type type)))
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x))
            (det (/ (,(compose-name #\/ 'mdet <s> <t>) m))))
        (macrolet ((e (y x) `(aref ma (+ ,x (* ,y ,',<s>))))
                   (f (&rest args)
                     (let ((gens (loop for i from 0 below (length args) collect (gensym (princ-to-string i)))))
                       `(let (,@(loop for arg in args for gen in gens collect (list gen arg)))
                          ,@(loop for g in gens
                                  for i from 0
                                  collect `(setf (aref xa ,i) ,g))
                          x))))
          ,(case <s>
             ;; FIXME: This *sucks*. Can't we compute this expansion?
             (2 `(f (* det (+ (e 1 1)))
                    (* det (- (e 0 1)))
                    (* det (- (e 1 0)))
                    (* det (+ (e 0 0)))))
             (3 `(f (* det (- (* (e 1 1) (e 2 2)) (* (e 1 2) (e 2 1))))
                    (* det (- (* (e 0 2) (e 2 1)) (* (e 0 1) (e 2 2))))
                    (* det (- (* (e 0 1) (e 1 2)) (* (e 0 2) (e 1 1))))
                    (* det (- (* (e 1 2) (e 2 0)) (* (e 1 0) (e 2 2))))
                    (* det (- (* (e 0 0) (e 2 2)) (* (e 0 2) (e 2 0))))
                    (* det (- (* (e 0 2) (e 1 0)) (* (e 0 0) (e 1 2))))
                    (* det (- (* (e 1 0) (e 2 1)) (* (e 1 1) (e 2 0))))
                    (* det (- (* (e 0 1) (e 2 0)) (* (e 0 0) (e 2 1))))
                    (* det (- (* (e 0 0) (e 1 1)) (* (e 0 1) (e 1 0))))))
             (4 `(f (* det (- (+ (* (e 1 2) (e 2 3) (e 3 1)) (* (e 1 3) (e 2 1) (e 3 2)) (* (e 1 1) (e 2 2) (e 3 3)))
                              (+ (* (e 1 3) (e 2 2) (e 3 1)) (* (e 1 1) (e 2 3) (e 3 2)) (* (e 1 2) (e 2 1) (e 3 3)))))
                    (* det (- (+ (* (e 0 3) (e 2 2) (e 3 1)) (* (e 0 1) (e 2 3) (e 3 2)) (* (e 0 2) (e 2 1) (e 3 3)))
                              (+ (* (e 0 2) (e 2 3) (e 3 1)) (* (e 0 3) (e 2 1) (e 3 2)) (* (e 0 1) (e 2 2) (e 3 3)))))
                    (* det (- (+ (* (e 0 2) (e 1 3) (e 3 1)) (* (e 0 3) (e 1 1) (e 3 2)) (* (e 0 1) (e 1 2) (e 3 3)))
                              (+ (* (e 0 3) (e 1 2) (e 3 1)) (* (e 0 1) (e 1 3) (e 3 2)) (* (e 0 2) (e 1 1) (e 3 3)))))
                    (* det (- (+ (* (e 0 3) (e 1 2) (e 2 1)) (* (e 0 1) (e 1 3) (e 2 2)) (* (e 0 2) (e 1 1) (e 2 3)))
                              (+ (* (e 0 2) (e 1 3) (e 2 1)) (* (e 0 3) (e 1 1) (e 2 2)) (* (e 0 1) (e 1 2) (e 2 3)))))
                    (* det (- (+ (* (e 1 3) (e 2 2) (e 3 0)) (* (e 1 0) (e 2 3) (e 3 2)) (* (e 1 2) (e 2 0) (e 3 3)))
                              (+ (* (e 1 2) (e 2 3) (e 3 0)) (* (e 1 3) (e 2 0) (e 3 2)) (* (e 1 0) (e 2 2) (e 3 3)))))
                    (* det (- (+ (* (e 0 2) (e 2 3) (e 3 0)) (* (e 0 3) (e 2 0) (e 3 2)) (* (e 0 0) (e 2 2) (e 3 3)))
                              (+ (* (e 0 3) (e 2 2) (e 3 0)) (* (e 0 0) (e 2 3) (e 3 2)) (* (e 0 2) (e 2 0) (e 3 3)))))
                    (* det (- (+ (* (e 0 3) (e 1 2) (e 3 0)) (* (e 0 0) (e 1 3) (e 3 2)) (* (e 0 2) (e 1 0) (e 3 3)))
                              (+ (* (e 0 2) (e 1 3) (e 3 0)) (* (e 0 3) (e 1 0) (e 3 2)) (* (e 0 0) (e 1 2) (e 3 3)))))
                    (* det (- (+ (* (e 0 2) (e 1 3) (e 2 0)) (* (e 0 3) (e 1 0) (e 2 2)) (* (e 0 0) (e 1 2) (e 2 3)))
                              (+ (* (e 0 3) (e 1 2) (e 2 0)) (* (e 0 0) (e 1 3) (e 2 2)) (* (e 0 2) (e 1 0) (e 2 3)))))
                    (* det (- (+ (* (e 1 1) (e 2 3) (e 3 0)) (* (e 1 3) (e 2 0) (e 3 1)) (* (e 1 0) (e 2 1) (e 3 3)))
                              (+ (* (e 1 3) (e 2 1) (e 3 0)) (* (e 1 0) (e 2 3) (e 3 1)) (* (e 1 1) (e 2 0) (e 3 3)))))
                    (* det (- (+ (* (e 0 3) (e 2 1) (e 3 0)) (* (e 0 0) (e 2 3) (e 3 1)) (* (e 0 1) (e 2 0) (e 3 3)))
                              (+ (* (e 0 1) (e 2 3) (e 3 0)) (* (e 0 3) (e 2 0) (e 3 1)) (* (e 0 0) (e 2 1) (e 3 3)))))
                    (* det (- (+ (* (e 0 1) (e 1 3) (e 3 0)) (* (e 0 3) (e 1 0) (e 3 1)) (* (e 0 0) (e 1 1) (e 3 3)))
                              (+ (* (e 0 3) (e 1 1) (e 3 0)) (* (e 0 0) (e 1 3) (e 3 1)) (* (e 0 1) (e 1 0) (e 3 3)))))
                    (* det (- (+ (* (e 0 3) (e 1 1) (e 2 0)) (* (e 0 0) (e 1 3) (e 2 1)) (* (e 0 1) (e 1 0) (e 2 3)))
                              (+ (* (e 0 1) (e 1 3) (e 2 0)) (* (e 0 3) (e 1 0) (e 2 1)) (* (e 0 0) (e 1 1) (e 2 3)))))
                    (* det (- (+ (* (e 1 2) (e 2 1) (e 3 0)) (* (e 1 0) (e 2 2) (e 3 1)) (* (e 1 1) (e 2 0) (e 3 2)))
                              (+ (* (e 1 1) (e 2 2) (e 3 0)) (* (e 1 2) (e 2 0) (e 3 1)) (* (e 1 0) (e 2 1) (e 3 2)))))
                    (* det (- (+ (* (e 0 1) (e 2 2) (e 3 0)) (* (e 0 2) (e 2 0) (e 3 1)) (* (e 0 0) (e 2 1) (e 3 2)))
                              (+ (* (e 0 2) (e 2 1) (e 3 0)) (* (e 0 0) (e 2 2) (e 3 1)) (* (e 0 1) (e 2 0) (e 3 2)))))
                    (* det (- (+ (* (e 0 2) (e 1 1) (e 3 0)) (* (e 0 0) (e 1 2) (e 3 1)) (* (e 0 1) (e 1 0) (e 3 2)))
                              (+ (* (e 0 1) (e 1 2) (e 3 0)) (* (e 0 2) (e 1 0) (e 3 1)) (* (e 0 0) (e 1 1) (e 3 2)))))
                    (* det (- (+ (* (e 0 1) (e 1 2) (e 2 0)) (* (e 0 2) (e 1 0) (e 2 1)) (* (e 0 0) (e 1 1) (e 2 2)))
                              (+ (* (e 0 2) (e 1 1) (e 2 0)) (* (e 0 0) (e 1 2) (e 2 1)) (* (e 0 1) (e 1 0) (e 2 2)))))))
             (T
              `(,(compose-name #\/ 'smatop '* '<t> <s> <t>)
                x (,(compose-name #\/ 'mcof <s> <t>) x m) det))))))))

(define-template mtransfer <sx> <s> <t> (x m w h xy xx my mx)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rett (type-instance 'mat-type <sx> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type ,(lisp-type rett) x)
               (type index xy xx my mx)
               (type dimension w h)
               (return-type ,(lisp-type rett)))
      (let* ((ma ,(place-form type 'arr 'm))
             (xa ,(place-form rett 'arr 'x))
             (mc ,(attribute type :cols 'm))
             (xc ,(attribute rett :cols 'x))
             (xi (+ xx (* xc xy)))
             (mi (+ mx (* mc my))))
        (declare (type index xi mi mc xc))
        (dotimes (_ h x)
          (dotimes (i w)
            (setf (aref xa (+ xi i)) (aref ma (+ mi i))))
          (setf xi (+ xi xc))
          (setf mi (+ mi mc)))))))

(define-template mswap-row <s> <t> (x m r1 r2)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (type index r1 r2)
               (return-type ,(lisp-type type)))
      (let* ((ma ,(place-form type 'arr 'm))
             (xa ,(place-form type 'arr 'x))
             (c ,(attribute type :cols 'm))
             (r1 (* c r1))
             (r2 (* c r2)))
        (unless (eq ma xa)
          (replace xa ma))
        (do-times (i 0 c 1 x)
          (rotatef (aref xa (+ r1 i)) (aref xa (+ r2 i))))))))

(define-template mswap-col <s> <t> (x m c1 c2)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (type index c1 c2)
               (return-type ,(lisp-type type)))
      (let* ((ma ,(place-form type 'arr 'm))
             (xa ,(place-form type 'arr 'x))
             (r ,(attribute type :rows 'm))
             (c ,(attribute type :cols 'm)))
        (unless (eq ma xa)
          (replace xa ma))
        (do-times (i 0 r 1 x)
          (rotatef (aref xa c1)
                   (aref xa c2))
          (incf c1 c)
          (incf c2 c))))))

(define-template mtranslate <s> <t> (x v)
  (when (member <s> '(2 n)) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>))
        (vtype (type-instance 'vec-type (1- <s>) <t>)))
    `((declare (type ,(lisp-type type) x)
               (type ,(lisp-type vtype) v)
               (return-type ,(lisp-type type)))
      (let ((va ,(place-form vtype 'arr 'v))
            (xa ,(place-form type 'arr 'x)))
        (macrolet ((e (x) `(aref va ,x))
                   (f (y x) `(aref xa (+ ,x (* ,y ,,(attribute type :cols 'x))))))
          ,@(loop for i from 0 below <s>
                  collect `(setf (f ,i ,(1- <s>))
                                 (,<t> (+ ,@(loop for j from 0 below <s>
                                                  collect `(* (f ,i ,j) ,(if (< j (1- <s>)) `(e ,j) 1)))))))
          x)))))

(define-template mscale <s> <t> (x v)
  (when (member <s> '(2 n)) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>))
        (vtype (type-instance 'vec-type (1- <s>) <t>)))
    `((declare (type ,(lisp-type type) x)
               (type ,(lisp-type vtype) v)
               (return-type ,(lisp-type type)))
      (let ((va ,(place-form vtype 'arr 'v))
            (xa ,(place-form type 'arr 'x)))
        (macrolet ((e (x) `(aref va ,x))
                   (f (y x) `(aref xa (+ ,x (* ,y ,,(attribute type :cols 'x))))))
          ,@(loop for i from 0 below <s>
                  append (loop for j from 0 below (1- <s>)
                               collect `(setf (f ,i ,j) (,<t> (* (f ,i ,j) (e ,j))))))
          x)))))

(define-template mrotate <s> <t> (x v angle)
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  (when (member <s> '(2 n)) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>))
        (vtype (type-instance 'vec-type (1- <s>) <t>)))
    `((declare (type ,(lisp-type type) x)
               (type ,(lisp-type vtype) v)
               (type ,<t> angle)
               (return-type ,(lisp-type type)))
      ;; KLUDGE: Cheap! Properly unroll at some point.
      (let ((tmp (,(lisp-type type))))
        (,(compose-name #\/ 'mrotation <s> <t>) tmp v angle)
        (,(compose-name #\/ 'm*m (lisp-type type) (lisp-type type)) x x tmp)))))

(defmacro define-generation-template (name args &body body)
  `(define-template ,name <s> <t> ,args
     (let ((type (type-instance 'mat-type <s> <t>)))
       (flet ((f (&rest args)
                (loop for arg in args
                      for i from 0 for x = (mod i 4) for y = (floor i 4)
                      when (and (< x <s>) (< y <s>))
                      collect `(setf (aref xa ,i) (,<t> ,arg)))))
         `((declare (type ,(lisp-type type) x)
                    (return-type ,(lisp-type type)))
           (let ((xa ,(place-form type 'arr 'x)))
             ,@(progn ,@body))
           x)))))

(define-generation-template mtranslation (x v)
  (when (member <s> '(2 n)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type (case <s> (4 3) (3 2)) <t>)))
    `((declare (type ,(lisp-type vtype) v))
      ,@(case <s>
          (3 (f 1 0 (place-form vtype :x 'v)
                0 1 (place-form vtype :y 'v)
                0 0 1))
          (4 (f 1 0 0 (place-form vtype :x 'v)
                0 1 0 (place-form vtype :y 'v)
                0 0 1 (place-form vtype :z 'v)
                0 0 0 1))))))

(define-generation-template mscaling (x v)
  (when (member <s> '(n)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type (case <s> (4 3) ((3 2) 2)) <t>)))
    `((declare (type ,(lisp-type vtype) v))
      ,@(case <s>
          (2 (f (place-form vtype :x 'v) 0
                0 (place-form vtype :y 'v)))
          (3 (f (place-form vtype :x 'v) 0 0
                0 (place-form vtype :y 'v) 0
                0 0 1))
          (4 (f (place-form vtype :x 'v) 0 0 0
                0 (place-form vtype :y 'v) 0 0
                0 0 (place-form vtype :z 'v) 0
                0 0 0 1))
          (T (template-unfulfillable))))))

(define-generation-template mrotation (x v angle)
  (when (member <s> '(n)) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type vtype) v)
               (type ,<t> angle))
      (let ((c (,<t> (cos angle)))
            (s (,<t> (sin angle)))
            (x ,(place-form vtype :x 'v))
            (y ,(place-form vtype :y 'v))
            (z ,(place-form vtype :z 'v)))
        ,@(case <s>
            (2 (f 'c '(- s)
                  's 'c))
            (T `((cond ((and (= 1 x) (= 0 y) (= 0 z))
                        ,@(f 1 0 0 0
                             0 'c '(- s) 0
                             0 's 'c 0
                             0 0 0 1))
                       ((and (= 0 x) (= 1 y) (= 0 z))
                        ,@(f 'c 0 's 0
                             0 1 0 0
                             '(- s) 0 'c 0
                             0 0 0 1))
                       ((and (= 0 x) (= 0 y) (= 1 z))
                        ,@(f 'c '(- s) 0 0
                             's 'c 0 0
                             0 0 1 0
                             0 0 0 1))
                       (T
                        ;; https://joombig.com/sqlc/3D-Rotation-Algorithm-about-arbitrary-axis-with-CC-code-tutorials-advance
                        (let* ((1-c (- 1 c))
                               (u2 (expt x 2))
                               (v2 (expt y 2))
                               (w2 (expt z 2))
                               (l (+ u2 v2 w2))
                               (sqrtl (sqrt l)))
                          ,@(f '(/ (+ u2 (* (+ v2 w2) c)) l)        '(/ (- (* x y 1-c) (* z sqrtl s)) l) '(/ (+ (* x z 1-c) (* y sqrtl s)) l) 0
                               '(/ (+ (* x y 1-c) (* z sqrtl s)) l) '(/ (+ v2 (* (+ u2 w2) c)) l)        '(/ (- (* y z 1-c) (* x sqrtl s)) l) 0
                               '(/ (- (* x z 1-c) (* y sqrtl s)) l) '(/ (+ (* y z 1-c) (* x sqrtl s)) l) '(/ (+ w2 (* (+ u2 v2) c)) l)        0
                               0                                   0                                   0                                   1)))))))))))

(define-generation-template mlookat (x eye target up)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type vtype) eye target up))
      (let* ((z (nvunit (v- eye target)))
             (x (if (and (= (abs (vx z)) (abs (vx up)))
                         (= (abs (vy z)) (abs (vy up)))
                         (= (abs (vz z)) (abs (vz up))))
                    (nvunit (vc (vec (vy up) (vz up) (vx up)) z))
                    (nvunit (vc up z))))
             (y (vc z x)))
        (declare (dynamic-extent z x y))
        ,@(f (place-form vtype :x 'x) (place-form vtype :y 'x) (place-form vtype :z 'x) '(- (v. x eye))
             (place-form vtype :x 'y) (place-form vtype :y 'y) (place-form vtype :z 'y) '(- (v. y eye))
             (place-form vtype :x 'z) (place-form vtype :y 'z) (place-form vtype :z 'z) '(- (v. z eye))
             0 0 0 1)))))

(define-generation-template mfrustum (x l r b u n f)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  `((declare (type ,<t> l r b u n f))
    ,@(f '(/ (* 2 n) (- r l)) 0                    '(/ (+ r l) (- r l))     0
         0                    '(/ (* 2 n) (- u b)) '(/ (+ u b) (- u b))     0
         0                    0                    '(- (/ (+ f n) (- f n))) '(/ (* -2 f n) (- f n))
         0                    0                    -1                       0)))

(define-template mperspective <s> <t> (x fovy aspect near far)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x)
               (type ,<t> fovy aspect near far)
               (return-type ,(lisp-type type)))
      (let* ((fh (* (the ,<t> (tan (* (/ fovy (,<t> 360)) (,<t> PI)))) near))
             (fw (* fh aspect)))
        (,(compose-name #\/ 'mfrustum <s> <t>) x (- fw) fw (- fh) fh near far)))))

(define-generation-template mortho (x l r b u n f)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  `((declare (type ,<t> l r b u n f))
    ,@(f '(/ 2 (- r l)) 0              0              '(- (/ (+ r l) (- r l)))
         0              '(/ 2 (- u b)) 0              '(- (/ (+ u b) (- u b)))
         0              0             '(/ -2 (- f n)) '(- (/ (+ f n) (- f n)))
         0              0              0              1)))

(define-template m1norm <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
      (let ((ma ,(place-form type 'arr 'm))
            (mc ,(attribute type :cols 'm))
            (max (,<t> 0)))
        (do-times (x 0 ,(attribute type :cols 'm) 1 max)
          (let ((col (,<t> 0)))
            (do-times (y 0 ,(attribute type :rows 'm) 1)
              (setf col (+ col (abs (aref ma (+ x (* y mc)))))))
            (when (< max col)
              (setf max col))))))))

(define-template minorm <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
      (let ((ma ,(place-form type 'arr 'm))
            (mc ,(attribute type :cols 'm))
            (max (,<t> 0)))
        (do-times (y 0 ,(attribute type :rows 'm) 1 max)
          (let ((col (,<t> 0)))
            (do-times (x 0 ,(attribute type :cols 'm) 1)
              (setf col (+ col (abs (aref ma (+ x (* y mc)))))))
            (when (< max col)
              (setf max col))))))))

(define-template m2norm <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
      (let ((ma ,(place-form type 'arr 'm))
            (sum (,<t> 0)))
        (do-times (i 0 ,(attribute type :len 'm) 1 (,<t> (sqrt sum)))
          (setf sum (+ sum (expt (aref ma i) 2))))))))

(define-template mtrace <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
      (let* ((ma ,(place-form type 'arr 'm))
             (mc ,(attribute type :cols 'm))
             (ret (,<t> 0))
             (mi 0))
        (dotimes (i (min ,(attribute type :cols 'm) ,(attribute type :rows 'm)) ret)
          (setf ret (+ ret (aref ma mi)))
          (incf mi (1+ mc)))))))

(define-template mdiag <s> <t> (r m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type (simple-array ,<t> (*)) r)
               (return-type (simple-array ,<t> (*))))
      (let* ((ma ,(place-form type 'arr 'm))
             (mc ,(attribute type :cols 'm))
             (mi 0))
        (do-times (i 0 (min ,(attribute type :rows 'm) ,(attribute type :cols 'm)) 1 r)
          (setf (aref r i) (aref ma mi))
          (incf mi (1+ mc)))))))

(define-template mrow <s> <t> (r m ri)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type (simple-array ,<t> (*)) r)
               (type index ri)
               (return-type (simple-array ,<t> (*))))
      (let* ((ma ,(place-form type 'arr 'm))
             (cs ,(attribute type :cols 'm))
             (ri (* cs ri)))
        (do-times (i 0 cs 1 r)
          (setf (aref r i) (aref ma (+ ri i))))))))

(define-template mcol <s> <t> (c m ci)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type (simple-array ,<t> (*)) c)
               (type index ci)
               (return-type (simple-array ,<t> (*))))
      (let* ((ma ,(place-form type 'arr 'm))
             (cs ,(attribute type :cols 'm))
             (rs ,(attribute type :rows 'm)))
        (do-times (i 0 rs 1 c)
          (setf (aref c i) (aref ma ci))
          (incf ci cs))))))

(do-type-combinations mat-type define-setf)
(do-type-combinations mat-type define-mapply)
(do-type-combinations mat-type define-mvec)
(do-type-combinations mat-type define-copy)
(do-type-combinations mat-type define-smatop (+ - * / min max) (<t> real))
(do-type-combinations mat-type define-2matop (+ - * / min max))
(do-type-combinations mat-type define-1matop (- / identity))
(do-type-combinations mat-type define-2matreduce (and) (= ~= /= < <= >= >) boolean)
(do-type-combinations mat-type define-smatreduce (and) (= ~= /= < <= >= >) (<t> real) boolean)
(do-type-combinations mat-type define-2matreduce (or) (/=) boolean)
(do-type-combinations mat-type define-smatreduce (or) (/=) (<t> real) boolean)
(do-type-combinations mat-type define-0matop (zero eye rand))
(do-type-combinations mat-type define-mdet)
(do-type-combinations mat-type define-minv)
(do-type-combinations mat-type define-mminor)
(do-type-combinations mat-type define-minv-affine)
(do-type-combinations mat-type define-mcof)
(do-type-combinations mat-type define-mtranspose)
(do-type-combinations mat-type define-mtrace)
(do-type-combinations mat-type define-mtransfer (2 3 4 n))
(do-type-combinations mat-type define-mswap-row)
(do-type-combinations mat-type define-mswap-col)
(do-type-combinations mat-type define-mtranslate)
(do-type-combinations mat-type define-mscale)
(do-type-combinations mat-type define-mrotate)
(do-type-combinations mat-type define-mtranslation)
(do-type-combinations mat-type define-mscaling)
(do-type-combinations mat-type define-mrotation)
(do-type-combinations mat-type define-mlookat)
(do-type-combinations mat-type define-mfrustum)
(do-type-combinations mat-type define-mperspective)
(do-type-combinations mat-type define-mortho)
(do-type-combinations mat-type define-m1norm)
(do-type-combinations mat-type define-minorm)
(do-type-combinations mat-type define-m2norm)
(do-type-combinations mat-type define-mdiag)
(do-type-combinations mat-type define-mrow)
(do-type-combinations mat-type define-mcol)

(do-instance-combinations define-m*m mat-type mat-type)
(do-instance-combinations define-m*m mat-type vec-type)
(do-instance-combinations define-m*m vec-type mat-type)

