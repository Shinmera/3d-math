#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.matrices)

(define-template-type mat (<s> <t>)
    (compose-name NIL (type-prefix <t>) 'mat <s>)
  (field (compose-name NIL (type-prefix <t>) 'marr <s>)
         :type `(simple-array ,<t> (,(if (integerp <s>) (* <s> <s>) '*)))
         :alias (list 0 'arr :arr))
  (cond ((eql <s> 'n)
         (field (compose-name NIL (type-prefix <t>) 'mrows <s>) :type 'dimension :alias '(1 rows))
         (field (compose-name NIL (type-prefix <t>) 'mcols <s>) :type 'dimension :alias '(2 cols)))
        (T
         (field (compose-name NIL (type-prefix <t>) 'mrows <s>) :type `(eql ,<s>) :alias '(1 rows) :value <s>)
         (field (compose-name NIL (type-prefix <t>) 'mcols <s>) :type `(eql ,<s>) :alias '(2 cols) :value <s>))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod compute-type-instance-definition ((type mat-type))
    `(progn
       ,(call-next-method)
       
       (defmethod print-object ((mat ,(lisp-type type)) stream)
         (let ((constructor ,(if (eql 'n (first (template-arguments type)))
                                 `(list ',(lisp-type type) (mrows mat) (mcols mat) (marr mat))
                                 `(list ',(lisp-type type) (marr mat)))))
           (write-constructor constructor stream))))))

(do-combinations define-mat (2 3 4 n)
  (#-3d-math-no-f32 f32
   #-3d-math-no-f64 f64
   #-3d-math-no-u32 u32
   #-3d-math-no-i32 i32))

#-3d-math-no-f32 (define-type-alias fmat mat2 mat3 mat4 matn)
#-3d-math-no-f64 (define-type-alias dmat dmat2 dmat3 dmat4 dmatn)
#-3d-math-no-i32 (define-type-alias imat imat2 imat3 imat4 imatn)
#-3d-math-no-u32 (define-type-alias umat umat2 umat3 umat4 umatn)
(define-type-alias *mat2
  #-3d-math-no-f32 mat2 #-3d-math-no-f64 dmat2 #-3d-math-no-i32 imat2 #-3d-math-no-u32 umat2)
(define-type-alias *mat3
  #-3d-math-no-f32 mat3 #-3d-math-no-f64 dmat3 #-3d-math-no-i32 imat3 #-3d-math-no-u32 umat3)
(define-type-alias *mat4
  #-3d-math-no-f32 mat4 #-3d-math-no-f64 dmat4 #-3d-math-no-i32 imat4 #-3d-math-no-u32 umat4)
(define-type-alias *matn
  #-3d-math-no-f32 matn #-3d-math-no-f64 dmatn #-3d-math-no-i32 imatn #-3d-math-no-u32 umatn)
(define-type-alias *mat
  #-3d-math-no-f32 mat2 #-3d-math-no-f64 dmat2 #-3d-math-no-i32 imat2 #-3d-math-no-u32 umat2
  #-3d-math-no-f32 mat3 #-3d-math-no-f64 dmat3 #-3d-math-no-i32 imat3 #-3d-math-no-u32 umat3
  #-3d-math-no-f32 mat4 #-3d-math-no-f64 dmat4 #-3d-math-no-i32 imat4 #-3d-math-no-u32 umat4
  #-3d-math-no-f32 matn #-3d-math-no-f64 dmatn #-3d-math-no-i32 imatn #-3d-math-no-u32 umatn)

(deftype mat (&optional s) 
  (case s
    ((NIL) 'fmat)
    (2 'mat2)
    (3 'mat3)
    (4 'mat4)))

(define-alias mat-p (thing)
  `(typep ,thing 'fmat))

(define-slot-accessor mat-type marr arr)
(define-slot-accessor mat-type mcols cols)
(define-slot-accessor mat-type mrows rows)

(defmacro define-mat-constructor (size type)
  (let ((name (compose-name NIL (type-prefix type) 'mat size))
        (lisp-type (lisp-type (type-instance 'mat-type size type))))
    (etypecase size
      ((eql n)
       `(progn
          (export '(,name))
          (define-type-dispatch ,name (n m &rest args)
            ((dimension dimension) ,lisp-type
             (,(constructor (type-instance 'mat-type size type))
              (map-into (make-array (* n m) :element-type ',type :initial-element (,type 0))
                        #',type args)
              n m))
            ((dimension dimension sequence) ,lisp-type
             (,(constructor (type-instance 'mat-type size type))
              (map-into (make-array (* n m) :element-type ',type :initial-element (,type 0))
                        #',type (first args))
              n m)))))
      (integer
       (let ((vec-type (type-instance 'vec-type size type))
             (args (loop for i from 0 below (* size size) collect (compose-name NIL 'v i))))
         (flet ((constructor (&rest args)
                  `(,(constructor (type-instance 'mat-type size type))
                     ,(if (rest args)
                          `(type-array ,(length args) ,type ,@args)
                          `(map-into (make-array ,(* size size) :element-type ',type)
                                     #',type ,(first args))))))
           `(progn
              (export '(,name))
              (define-type-dispatch ,name (&optional ,@args)
                (,(loop repeat (length args) collect 'null) ,lisp-type
                 ,(apply #'constructor (make-list (length args) :initial-element 0)))
                (,(loop repeat (length args) collect 'real) ,lisp-type
                 ,(apply #'constructor args))
                ((real ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 ,(apply #'constructor (make-list (length args) :initial-element (first args))))
                ((,lisp-type ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 (mcopy ,(first args)))
                ((,(compose-name NIL '* 'mat size) ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 ,(constructor `(marr ,(first args))))
                ((sequence ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 ,(constructor (first args)))
                ((,@(loop repeat size collect (lisp-type vec-type))
                  ,@(loop repeat (- (length args) size) collect 'null)) ,lisp-type
                 ,(apply #'constructor (loop for i from 0 below size
                                             append (loop for arg in args repeat size
                                                          collect (place-form vec-type i arg)))))))))))))

(defmacro define-mat*-constructor (type)
  (let ((name (compose-name NIL (type-prefix type) 'mat))
        (args (loop for i from 0 below (* 4 4) collect (compose-name NIL 'v i)))
        (mat* (ecase type
                (f32 'fmat)
                (f64 'dmat)
                (i32 'imat)
                (u32 'umat))))
    (labels ((make (size args)
               `(,(constructor (type-instance 'mat-type size type))
                  ,(if (rest args)
                       `(type-array ,(length args) ,type ,@args)
                       `(map-into (make-array ,(* size size) :element-type ',type)
                                  #',type ,(first args)))))
             (args (n)
               (append (loop repeat (* n n) collect 'real)
                       (loop repeat (- (* 4 4) (* n n)) collect 'null)))
             (make-vector (n)
               `(,(constructor (type-instance 'mat-type n type))
                  (map-into (make-array ,(* n n) :element-type ',type)
                            #',type ,(first args))))
             (case-vec (n)
               (let ((vec-type (type-instance 'vec-type n type)))
                 `((,@(loop repeat n collect (lisp-type vec-type))
                    ,@(loop repeat (- (length args) n) collect 'null))
                   ,(lisp-type (type-instance 'mat-type n type))
                   ,(make n (loop for i from 0 below n
                                  append (loop for arg in args repeat n
                                               collect (place-form vec-type i arg))))))))
      `(progn
         (export '(,name))
         (define-type-dispatch ,name (&optional ,@args)
           ((dimension dimension) ,mat*
            (cond ((/= ,(first args) ,(second args))
                   (,(lisp-type (type-instance 'mat-type 'n type)) ,(first args) ,(second args)))
                  ((= ,(first args) 2)
                   (,(lisp-type (type-instance 'mat-type 2 type))))
                  ((= ,(first args) 3)
                   (,(lisp-type (type-instance 'mat-type 3 type))))
                  ((= ,(first args) 4)
                   (,(lisp-type (type-instance 'mat-type 4 type))))
                  (T
                   (,(lisp-type (type-instance 'mat-type 'n type)) ,(first args) ,(second args)))))
           ((dimension dimension sequence) ,mat*
            (cond ((/= ,(first args) ,(second args))
                   (,(lisp-type (type-instance 'mat-type 'n type)) ,(first args) ,(second args) ,(third args)))
                  ((= ,(first args) 2)
                   (,(lisp-type (type-instance 'mat-type 2 type)) ,(third args)))
                  ((= ,(first args) 3)
                   (,(lisp-type (type-instance 'mat-type 3 type)) ,(third args)))
                  ((= ,(first args) 4)
                   (,(lisp-type (type-instance 'mat-type 4 type)) ,(third args)))
                  (T
                   (,(lisp-type (type-instance 'mat-type 'n type)) ,(first args) ,(second args) ,(third args)))))
           ((dimension) ,mat*
            (cond ((= ,(first args) 2)
                   (,(lisp-type (type-instance 'mat-type 2 type))))
                  ((= ,(first args) 3)
                   (,(lisp-type (type-instance 'mat-type 3 type))))
                  ((= ,(first args) 4)
                   (,(lisp-type (type-instance 'mat-type 4 type))))
                  (T
                   (,(lisp-type (type-instance 'mat-type 'n type)) ,(first args) ,(first args)))))
           (,(args 2) ,(lisp-type (type-instance 'mat-type 2 type))
            ,(make 2 (subseq args 0 4)))
           (,(args 3) ,(lisp-type (type-instance 'mat-type 3 type))
            ,(make 3 (subseq args 0 9)))
           (,(args 4) ,(lisp-type (type-instance 'mat-type 4 type))
            ,(make 4 args))
           ((mat ,@(loop repeat 15 collect 'null)) *mat
            (mcopy ,(first args)))
           ((vector ,@(loop repeat 15 collect 'null)) ,mat*
            (ecase (length ,(first args))
              (2 ,(make-vector 2))
              (3 ,(make-vector 3))
              (4 ,(make-vector 4))))
           ,(case-vec 2)
           ,(case-vec 3)
           ,(case-vec 4))))))

(do-type-combinations mat-type define-mat-constructor)
(do-combinations define-mat*-constructor
  (#-3d-math-no-f32 f32
   #-3d-math-no-f64 f64
   #-3d-math-no-u32 u32
   #-3d-math-no-i32 i32))

(defmacro define-mat-describe (type)
  `(defmethod describe-object ((mat ,(lisp-type type)) stream)
     (format stream "~&~s:~%" ',(lisp-type type))
     (write-matrix mat stream)
     (fresh-line stream)))

(do-instance-combinations define-mat-describe mat-type)

(defun write-matrix (m stream &key (format :nice))
  (etypecase stream
    (null (with-output-to-string (out)
            (write-matrix m out :format format)))
    ((eql T) (write-matrix m *standard-output* :format format))
    (stream
     (labels ((val (value)
                (etypecase value
                  (integer (format stream "~d" value))
                  (single-float (format stream "~f" value))
                  (double-float (format stream "~f" value))))
              (simple (prefix suffix &optional (row-prefix "") (row-suffix "") (value-separator ", ") (row-separator value-separator))
                (write-string prefix stream)
                (dotimes (i (mrows m))
                  (write-string row-prefix stream)
                  (dotimes (j (mcols m))
                    (val (mcref m i j))
                    (unless (= j (1- (mcols m))) (write-string value-separator stream)))
                  (write-string row-suffix stream)
                  (unless (= i (1- (mrows m)))
                    (write-string row-separator stream)))
                (write-string suffix stream)))
       (ecase format
         (:nice
          (dotimes (i (mrows m))
            (cond ((or (= (mrows m) 1) (< 0 i (1- (mrows m))))
                   (write-string "│ " stream))
                  ((= i 0)
                   (write-string "┌ " stream))
                  (T
                   (write-string "└ " stream)))
            (dotimes (j (mcols m))
              (format stream "~10,3@e " (mcref m i j)))
            (cond ((or (= (mrows m) 1) (< 0 i (1- (mrows m))))
                   (write-string "│" stream))
                  ((= i 0)
                   (write-string "┐" stream))
                  (T
                   (write-string "┘" stream)))
            (unless (= i (1- (mrows m)))
              (terpri stream))))
         (:wolfram
          (simple "{" "}" "{" "}"))
         ((:lisp :array)
          (simple "#2A(" ")" "(" ")" " "))
         (:c
          (simple "{" "}" "" ""))
         (:json
          (simple "[" "]" "" ""))))
     m)))


