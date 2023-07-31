#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(defpackage #:org.shirakumo.fraf.math.internal
  (:use #:cl #:org.shirakumo.type-templates)
  ;; template types
  (:export
   #:vec-type
   #:mat-type
   #:quat-type
   #:dquat-type
   #:transform-type)
  ;; toolkit.lisp
  (:export
   #:*matrix-limit*
   #:enlist
   #:f32
   #:f64
   #:u32
   #:i32
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
   #:define-exports))

(defpackage #:org.shirakumo.fraf.math.vectors
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shiarkumo.fraf.math.internal)
  (:import-from #:org.shirakumo.type-templates #:dbg #:lambda-list-variables))

(defpackage #:org.shirakumo.fraf.math.matrices
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shiarkumo.fraf.math.internal
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames
   (#:v #:org.shirakumo.fraf.vectors)))

(defpackage #:org.shirakumo.fraf.math.quaternions
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shiarkumo.fraf.math.internal
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices))

(defpackage #:org.shirakumo.fraf.math.transforms
  (:use #:cl
        #:org.shirakumo.type-templates
        #:org.shiarkumo.fraf.math.internal
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:org.shirakumo.fraf.math.quaternions))

(defpackage #:org.shirakumo.fraf.math
  (:use #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:org.shirakumo.fraf.math.quaternions
        #:org.shirakumo.fraf.math.transforms))
