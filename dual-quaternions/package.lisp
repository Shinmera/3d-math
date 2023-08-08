#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.dual-quaternions)

(define-exports
  ;; types.lisp
  ;; Much is omitted, exported by autogen.
  #:q2real
  #:q2dual
  #:*quat2
  #:quat2-p
  #:q2copy
  #:q2zero
  ;; ops.lisp
  #:q2<-
  #:!q2conjugate
  #:!q2unit
  #:!q2unit*
  #:!q2+
  #:!q2*
  #:!q2-
  #:!q2/
  #:!q2location
  #:!q2from-location
  #:nq2+
  #:nq2-
  #:nq2*
  #:nq2/
  #:nq2conjugate
  #:nq2unit
  #:nq2unit*
  #:q2+
  #:q2-
  #:q2*
  #:q2/
  #:q2=
  #:q2~=
  #:q2/=
  #:q2conjugate
  #:q2unit
  #:q2unit*
  #:q2location
  #:q2from-location
  #:q2.
  #:q2sqrlength
  #:q2length)
