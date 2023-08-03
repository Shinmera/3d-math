#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

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
   #:dquat-type
   #:transform-type
   #:arr)
  ;; toolkit.lisp
  (:export
   #:*matrix-limit*
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
   #:define-type-reductor
   #:define-value-reductor
   #:define-pure-alias
   #:define-modifying-alias
   #:define-simple-alias
   #:define-rest-alias))

(defpackage #:org.shirakumo.fraf.math.vectors
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shirakumo.fraf.math.internal))

(defpackage #:org.shirakumo.fraf.math.matrices
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shirakumo.fraf.math.internal
        #:org.shirakumo.fraf.math.vectors))

(defpackage #:org.shirakumo.fraf.math.quaternions
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shirakumo.fraf.math.internal
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices))

(defpackage #:org.shirakumo.fraf.math.transforms
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shirakumo.fraf.math.internal
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:org.shirakumo.fraf.math.quaternions))

(defpackage #:org.shirakumo.fraf.math
  (:use #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:org.shirakumo.fraf.math.quaternions
        #:org.shirakumo.fraf.math.transforms))
