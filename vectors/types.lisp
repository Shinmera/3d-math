#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.vectors)

(defmacro %vec-array (<s> <t> &rest values)
  (let ((array (gensym "ARRAY")))
    `(let ((,array (make-array ,<s> :element-type ',<t>)))
       ,@(loop for i from 0 below <s>
               for v in values
               collect `(setf (aref ,array ,i) (,<t> ,v)))
       ,array)))

(define-template-type vec (<s> <t>)
    (compose-name NIL (type-prefix <t>) 'vec <s>)
  (let ((varr (compose-name NIL (type-prefix <t>) 'varr <s>)))
    (field varr
           :type `(simple-array ,<t> (,<s>))
           :alias (list 'arr :arr))
    (loop for i from 0 below <s>
          for f in '(x y z w)
          do (field (compose-name NIL (type-prefix <t>) 'v f <s>)
                    :type <t>
                    :alias (list i f (intern (string f) "KEYWORD"))
                    :computed T
                    :value `(lambda (vec) `(aref (,',varr ,vec) ,,i))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod compute-type-instance-definition ((type vec-type))
    `(progn
       ,(call-next-method)
       (defmethod print-object ((vec ,(lisp-type type)) stream)
         (write (list ',(lisp-type type)
                      ,@(loop for slot in (slots type)
                              unless (realized-slot-p slot)
                              collect `(,(accessor slot) vec)))
                :stream stream)))))

(do-combinations define-vec (2 3 4)
  (#-3d-math-no-f32 f32
   #-3d-math-no-f64 f64
   #-3d-math-no-u32 u32
   #-3d-math-no-i32 i32))

(define-slot-accessor vec-type vx 0)
(define-slot-accessor vec-type vy 1)
(define-slot-accessor vec-type vz 2)
(define-slot-accessor vec-type vw 3)
(define-slot-accessor vec-type varr arr)

#-3d-math-no-f32 (define-type-alias fvec vec2 vec3 vec4)
#-3d-math-no-f64 (define-type-alias dvec dvec2 dvec3 dvec4)
#-3d-math-no-i32 (define-type-alias ivec ivec2 ivec3 ivec4)
#-3d-math-no-u32 (define-type-alias uvec uvec2 uvec3 uvec4)
(define-type-alias *vec2
  #-3d-math-no-f32 vec2 #-3d-math-no-f64 dvec2 #-3d-math-no-i32 ivec2 #-3d-math-no-u32 uvec2)
(define-type-alias *vec3
  #-3d-math-no-f32 vec3 #-3d-math-no-f64 dvec3 #-3d-math-no-i32 ivec3 #-3d-math-no-u32 uvec3)
(define-type-alias *vec4
  #-3d-math-no-f32 vec4 #-3d-math-no-f64 dvec4 #-3d-math-no-i32 ivec4 #-3d-math-no-u32 uvec4)
(define-type-alias *vec
  #-3d-math-no-f32 vec2 #-3d-math-no-f64 dvec2 #-3d-math-no-i32 ivec2 #-3d-math-no-u32 uvec2
  #-3d-math-no-f32 vec3 #-3d-math-no-f64 dvec3 #-3d-math-no-i32 ivec3 #-3d-math-no-u32 uvec3
  #-3d-math-no-f32 vec4 #-3d-math-no-f64 dvec4 #-3d-math-no-i32 ivec4 #-3d-math-no-u32 uvec4)

(deftype vec (&optional (s NIL))
  (case s
    ((NIL single-float) 'fvec)
    (double-float 'dvec)
    (integer 'ivec)
    (2 'vec2)
    (3 'vec3)
    (4 'vec4)
    (* '*vec)))

(define-alias vec-p (thing)
  `(typep ,thing 'fvec))

;; This reads like a war zone
;; FIXME: The deftransforms on REALs clobber more precise type information.
(defmacro define-vec-constructors (type)
  (flet ((constructor (&rest args)
           `(,(constructor (type-instance 'vec-type (length args) type))
              (%vec-array ,(length args) ,type ,@args)))
         (type (size)
           (lisp-type (type-instance 'vec-type size type)))
         (place (size i)
           (place (type-instance 'vec-type size type) i)))
    (let ((2-name (compose-name NIL (type-prefix type) 'vec 2))
          (3-name (compose-name NIL (type-prefix type) 'vec 3))
          (4-name (compose-name NIL (type-prefix type) 'vec 4))
          (*-name (compose-name NIL (type-prefix type) 'vec)))
      `(progn
         (export '(,2-name ,3-name ,4-name ,*-name))
         (define-type-dispatch ,2-name (&optional a b)
           (() ,(type 2)
            ,(constructor '0 '0))
           ((real real) ,(type 2)
            ,(constructor 'a 'b))
           ((real) ,(type 2)
            ,(constructor 'a 'a))
           ((,(type 2)) ,(type 2)
            (,(compose-name NIL (type-prefix type) 'vec 2 '-copy) a))
           ((*vec2) ,(type 2)
            ,(constructor '(vx a) '(vy a))))

         (define-type-dispatch ,3-name (&optional a b c)
           (() ,(type 3)
            ,(constructor '0 '0 '0))
           ((real real real) ,(type 3)
            ,(constructor 'a 'b 'c))
           ((real) ,(type 3)
            ,(constructor 'a 'a 'a))
           ((,(type 2) real) ,(type 3)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a) 'b))
           ((real ,(type 2)) ,(type 3)
            ,(constructor 'a `(,(place 2 'x) b) `(,(place 2 'y) b)))
           ((,(type 3)) ,(type 3)
            (,(compose-name NIL (type-prefix type) 'vec 3 '-copy) a))
           ((*vec3) ,(type 3)
            ,(constructor '(vx a) '(vy a) '(vz a))))

         (define-type-dispatch ,4-name (&optional a b c d)
           (() ,(type 4)
            ,(constructor '0 '0 '0 '0))
           ((real real real real) ,(type 4)
            ,(constructor 'a 'b 'c 'd))
           ((real) ,(type 4)
            ,(constructor 'a 'a 'a 'a))
           ((,(type 2) ,(type 2) null null) ,(type 4)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a) `(,(place 2 'x) b) `(,(place 2 'y) b)))
           ((,(type 2) real real) ,(type 4)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a) 'b 'c))
           ((real ,(type 2) real) ,(type 4)
            ,(constructor 'a `(,(place 2 'x) b) `(,(place 2 'y) b) 'c))
           ((real real ,(type 2)) ,(type 4)
            ,(constructor 'a 'b `(,(place 2 'x) c) `(,(place 2 'y) c)))
           ((,(type 3) real) ,(type 4)
            ,(constructor `(,(place 3 'x) a) `(,(place 3 'y) a) `(,(place 3 'z) a) 'b))
           ((real ,(type 3)) ,(type 4)
            ,(constructor 'a `(,(place 3 'x) b) `(,(place 3 'y) b) `(,(place 3 'z) b)))
           ((,(type 4)) ,(type 4)
            (,(compose-name NIL (type-prefix type) 'vec 4 '-copy) a))
           ((*vec4) ,(type 4)
            ,(constructor '(vx a) '(vy a) '(vz a) '(vw a))))

         (define-type-dispatch ,*-name (&optional a b c d)
           ((dimension null null null) ,(case type (f32 'fvec) (f64 'dvec) (i32 'ivec) (u32 'uvec))
            (cond ((= a 2)
                   (,(lisp-type (type-instance 'vec-type 2 type))))
                  ((= a 3)
                   (,(lisp-type (type-instance 'vec-type 3 type))))
                  ((= a 4)
                   (,(lisp-type (type-instance 'vec-type 4 type))))
                  (T
                   (error "Not a valid size for a vector: ~d" a))))
           ((real real) ,(type 2)
            ,(constructor 'a 'b))
           ((real real real) ,(type 3)
            ,(constructor 'a 'b 'c))
           ((real real real real) ,(type 4)
            ,(constructor 'a 'b 'c 'd))
           ((,(type 2) real) ,(type 3)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a) 'b))
           ((real ,(type 2)) ,(type 3)
            ,(constructor 'a `(,(place 2 'x) b) `(,(place 2 'y) b)))
           ((,(type 2) ,(type 2)) ,(type 4)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a) `(,(place 2 'x) b) `(,(place 2 'y) b)))
           ((,(type 2) real real) ,(type 4)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a) 'b 'c))
           ((real ,(type 2) real) ,(type 4)
            ,(constructor 'a `(,(place 2 'x) b) `(,(place 2 'y) b) 'c))
           ((real real ,(type 2)) ,(type 4)
            ,(constructor 'a 'b `(,(place 2 'x) c) `(,(place 2 'y) c)))
           ((,(type 3) real) ,(type 4)
            ,(constructor `(,(place 3 'x) a) `(,(place 3 'y) a) `(,(place 3 'z) a) 'b))
           ((real ,(type 3)) ,(type 4)
            ,(constructor 'a `(,(place 3 'x) b) `(,(place 3 'y) b) `(,(place 3 'z) b)))
           ((,(type 2)) ,(type 2)
            ,(constructor `(,(place 2 'x) a) `(,(place 2 'y) a)))
           ((,(type 3)) ,(type 3)
            ,(constructor `(,(place 3 'x) a) `(,(place 3 'y) a) `(,(place 3 'z) a)))
           ((,(type 4)) ,(type 4)
            ,(constructor `(,(place 4 'x) a) `(,(place 4 'y) a) `(,(place 4 'z) a) `(,(place 4 'w) a)))
           ((*vec2) ,(type 2)
            ,(constructor '(vx a) '(vy a)))
           ((*vec3) ,(type 3)
            ,(constructor '(vx a) '(vy a) '(vz a)))
           ((*vec4) ,(type 4)
            ,(constructor '(vx a) '(vy a) '(vz a) '(vw a))))))))

#-3d-math-no-f32 (define-vec-constructors f32)
#-3d-math-no-f64 (define-vec-constructors f64)
#-3d-math-no-u32 (define-vec-constructors u32)
#-3d-math-no-i32 (define-vec-constructors i32)

(macrolet ((emit ()
             `(define-type-dispatch vcopy (a)
                ,@(loop for instance in (instances 'vec-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance)
                                   (make-array ,(<s> instance) :element-type ',(<t> instance)
                                                               :initial-contents ,(place-form instance :arr 'a))))))))
  (emit))

(macrolet ((emit ()
             `(define-type-dispatch vzero (a)
                ,@(loop for instance in (instances 'vec-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance)
                                  (,(constructor instance) (make-array ,(<s> instance) :element-type ',(<t> instance)
                                                                                       :initial-element (,(<t> instance) 0))))))))
  (emit))

(defmacro with-vec ((x y &optional z w) val &body body)
  (let ((valg (gensym "VAL"))
        (vars (delete-if #'null (list x y z w))))
    (flet ((bind (type)
             (let ((vars vars)
                   (slots (remove-if #'realized-slot-p (slots type))))
               `(symbol-macrolet (,@(loop for slot in slots
                                          for var = (pop vars)
                                          when var
                                          collect `(,var (,(accessor slot) ,valg)))
                                  ,@(loop for var in vars
                                          when var
                                          collect `(,var (,(lisp-type (first slots)) 0))))
                  ,@body))))
      `(let ((,valg ,val))
         (etypecase ,valg
           ,@(loop for type in (instances 'vec-type)
                   collect `(,(lisp-type type) ,(bind type))))))))
