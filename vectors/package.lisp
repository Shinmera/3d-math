#|
This file is a part of 3d-math
(c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.vectors)

(define-exports
  ;; types.lisp
  ;; Much is omitted, exported by autogen.
  #:vx
  #:vy
  #:vz
  #:vw
  #:varr
  #:fvec
  #:dvec
  #:ivec
  #:uvec
  #:*vec
  #:*vec2
  #:*vec3
  #:*vec4
  #:vec-p
  #:vcopy
  #:vzero
  #:with-vec
  #:write-vector
  ;; ops.lisp
  #:vsetf
  #:v<-
  #:!valign
  #:!1v-
  #:!1v/
  #:!vabs
  #:!v+
  #:!v*
  #:!v-
  #:!v/
  #:!vmin
  #:!vmax
  #:!v+*
  #:!vinv
  #:!vclamp
  #:!vlerp
  #:!vfloor
  #:!vround
  #:!vceiling
  #:!vrand
  #:!vload
  #:!vstore
  #:!vc
  #:!vrot
  #:!vrot2
  #:!vcartesian
  #:!vpolar
  #:!vapply
  #:!vunit
  #:!vunit*
  #:v=
  #:v~=
  #:v/=
  #:v<
  #:v<=
  #:v>
  #:v>=
  #:v.
  #:vdistance
  #:vsqrdistance
  #:v1norm
  #:vinorm
  #:v2norm
  #:vpnorm
  #:vsqrlength
  #:vlike
  #:v+
  #:v-
  #:v*
  #:v/
  #:vmin
  #:vmax
  #:vabs
  #:vmod
  #:vfloor
  #:vceiling
  #:vround
  #:vc
  #:vrot
  #:vrot2
  #:valign
  #:vcartesian
  #:vpolar
  #:vlerp
  #:vrand
  #:vapply
  #:nvapply
  #:vorder
  #:vunit
  #:vunit*
  #:vrotv
  #:vscale
  #:vincf
  #:vdecf
  #:vlength
  #:v1+
  #:v1-
  #:vangle
  #:vclamp
  #:v+*
  #:vinv
  #:vproject
  #:!vproject
  #:nvproject
  #:vref
  #:vx-angle
  #:vy-angle
  #:vz-angle
  #:nvinv
  #:nv+
  #:nv-
  #:nv*
  #:nv/
  #:nvmin
  #:nvmax
  #:nvabs
  #:nvmod
  #:nvfloor
  #:nvceiling
  #:nvround
  #:nvc
  #:nvrot
  #:nvrot2
  #:nvalign
  #:nvcartesian
  #:nvpolar
  #:nvlerp
  #:nvrand
  #:nvorder
  #:nvunit
  #:nvunit*
  #:nvrotv
  #:nvscale
  #:nvclamp
  #:nv+*
  ;; Swizzlers are exported by autogen.
  #:+vx2+
  #:+vy2+
  #:+vx3+
  #:+vy3+
  #:+vz3+
  #:+vx4+
  #:+vy4+
  #:+vz4+
  #:+vw4+
  #:+vx+
  #:+vy+
  #:+vz+
  #:-vx2+
  #:-vy2+
  #:-vx3+
  #:-vy3+
  #:-vz3+
  #:-vx4+
  #:-vy4+
  #:-vz4+
  #:-vw4+
  #:-vx+
  #:-vy+
  #:-vz+)
