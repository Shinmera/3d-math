#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-3d-math-u32 (push :3d-math-no-u32 *features*)
  #-3d-math-no-f32 (push :3d-math-f32 *features*)
  #-3d-math-no-f64 (push :3d-math-f64 *features*)
  #-3d-math-no-i32 (push :3d-math-i32 *features*))

(defpackage #:org.shirakumo.fraf.math.internal
  (:use #:cl #:org.shirakumo.type-templates)
  (:import-from #:org.shirakumo.type-templates #:dbg #:lambda-list-variables)
  (:export
   #:dbg
   #:lambda-list-variables)
  ;; template types
  (:export
   #:<t>
   #:<s>
   #:vec-type
   #:mat-type
   #:quat-type
   #:quat2-type
   #:transform-type
   #:arr
   #:*zero)
  ;; toolkit.lisp
  (:export
   #:*matrix-limit*
   #:dimension
   #:index
   #:enlist
   #:f32
   #:f64
   #:u32
   #:i32
   #:type-array
   #:type-prefix
   #:sqr
   #:sqr2
   #:grid
   #:sqrt+
   #:lerp
   #:clamp
   #:type-random
   #:~=
   #:ensure-function
   #:do-times
   #:zero
   #:eye
   #:rand
   #:define-exports
   #:do-type-combinations
   #:do-instance-combinations
   #:define-type-reductor
   #:define-value-reductor
   #:define-pure-alias
   #:define-modifying-alias
   #:define-simple-alias
   #:define-rest-alias
   #:define-slot-accessor))

(unless (find-package '#:org.shirakumo.fraf.math.vectors)
  (make-package '#:org.shirakumo.fraf.math.vectors
                :use '(#:cl
                       #:org.shirakumo.type-templates
                       #:org.shirakumo.fraf.math.internal))

  (make-package '#:org.shirakumo.fraf.math.matrices
                :use '(#:cl
                       #:org.shirakumo.type-templates
                       #:org.shirakumo.fraf.math.internal
                       #:org.shirakumo.fraf.math.vectors))

  (make-package '#:org.shirakumo.fraf.math.quaternions
                :use '(#:cl
                       #:org.shirakumo.type-templates
                       #:org.shirakumo.fraf.math.internal
                       #:org.shirakumo.fraf.math.vectors
                       #:org.shirakumo.fraf.math.matrices))

  (make-package '#:org.shirakumo.fraf.math.dual-quaternions
                :use '(#:cl
                       #:org.shirakumo.type-templates
                       #:org.shirakumo.fraf.math.internal
                       #:org.shirakumo.fraf.math.vectors
                       #:org.shirakumo.fraf.math.matrices
                       #:org.shirakumo.fraf.math.quaternions))

  (make-package '#:org.shirakumo.fraf.math.transforms
                :use '(#:cl
                       #:org.shirakumo.type-templates
                       #:org.shirakumo.fraf.math.internal
                       #:org.shirakumo.fraf.math.vectors
                       #:org.shirakumo.fraf.math.matrices
                       #:org.shirakumo.fraf.math.quaternions
                       #:org.shirakumo.fraf.math.dual-quaternions))

  (make-package '#:org.shirakumo.fraf.math
                :use '(#:org.shirakumo.fraf.math.vectors
                       #:org.shirakumo.fraf.math.matrices
                       #:org.shirakumo.fraf.math.quaternions
                       #:org.shirakumo.fraf.math.transforms
                       #:org.shirakumo.fraf.math.dual-quaternions)))
