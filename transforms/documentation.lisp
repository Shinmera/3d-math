#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.transforms)

(docs:define-docs
  (type transform
    "A single-float transform.

See *TRANSFORM (type)
See TRANSFORM
See TLOCATION
See TSCALING
See TROTATION")
  
  (type dtransform
    "A double-float transform.

See *TRANSFORM (type)
See TRANSFORM
See TLOCATION
See TSCALING
See TROTATION")
  
  (type *transform
    "Type encapsulating all transforms of any element type.

See TRANSFORM (type)
See DTRANSFORM (type)")
  
  (function tlocation
    "Accesses the location vector of the transform.

See *TRANSFORM (type)
See *VEC3 (type)")
  
  (function tscaling
    "Accesses the scaling vector of the transform.

See *TRANSFORM (type)
See *VEC3 (type)")
  
  (function trotation
    "Accesses the rotation quaternion of the transform.

See *TRANSFORM (type)
See *QUAT (type)")
  
  (function transform
    "Construct a single-float transform.

You may initialise the transform as follows:
- No arguments construct a neutral transform
- A VEC3 is used for the location
- Two VEC3s are used for the location and scaling
- Two VEC3s and a QUAT fully initialise the transform
- A QUAT is used for the rotation
- A VEC3 and a QUAT are used for the location and rotation
- Another TRANSFORM creates a copy

See QUAT (type)
See VEC3 (type)
See *TRANSFORM (type)
See TRANSFORM (type)")
  
  (function dtransform
    "Construct a double-float transform.

You may initialise the transform as follows:
- No arguments construct a neutral transform
- A DVEC3 is used for the location
- Two DVEC3s are used for the location and scaling
- Two DVEC3s and a DQUAT fully initialise the transform
- A DQUAT is used for the rotation
- A DVEC3 and a DQUAT are used for the location and rotation
- Another TRANSFORM creates a copy

See DQUAT (type)
See DVEC3 (type)
See *TRANSFORM (type)
See DTRANSFORM (type)")
  
  (function tcopy
    "Creates a copy of the transform.

The contained elements are not shared with the resulting transform.

See *TRANSFORM (type)")
  
  (function tzero
    "Creates a copy of the transform initialised to neutral.

The contained elements are not shared with the resulting transform.

See *TRANSFORM (type)"))

(docs:define-docs
  (function t=
    "Checks whether the passed transforms are identical in all elements.

The elements are checked \"in parallel\", meaning the LOCATION element
of each transform is only checked against the LOCATION element of others,
etc.

See T~=
See T/=
See *TRANSFORM (type)")
  
  (function t~=
    "Checks whether the passed transforms are equal in all elements.

The elements are checked \"in parallel\", meaning the LOCATION element
of each transform is only checked against the LOCATION element of others,
etc.

See ~=
See T~=
See T/=
See *TRANSFORM (type)")
  
  (function t/=
    "Checks whether the passed transforms are different in any elements.

The elements are checked \"in parallel\", meaning the LOCATION element
of each transform is only checked against the LOCATION element of others,
etc.

See T=
See T~=
See *TRANSFORM (type)")
  
  (function t+
    "Combines the two transforms together and returns a fresh transform.

See *TRANSFORM (type)")
  
  (function t-
    "\"Subtracts\" the transform and returns a fresh one.

This is semantically the same as (T+ a (TINV b))

See *TRANSFORM (type)")
  
  (function t*v
    "Multiplies the vector by the transform and returns a fresh vector.

This applies scaling and rotation, but not translation.

See *TRANSFORM (type)
See *VEC3 (type)")
  
  (function t*p
    "Multiplies the point by the transform and returns a fresh vector.

This applies scaling, rotation, and translation.

See *TRANSFORM (type)
See *VEC3 (type)")
  
  (function t*p-inv
    "Multiplies the point by the inverse of the transform and returns a fresh vector.

This is semantically the same as (T*P (TINV a) b)

See *TRANSFORM (type)
See *VEC3 (type)")
  
  (function tinv
    "Computes the inverses of the transform.

See !TINV
See NTINV
See *TRANSFORM (type)")
  
  (function tmix
    "Computes the linear mix of the two transforms.

See !TMIX
See NTMIX
See *TRANSFORM (type)")
  
  (function tmat
    "Computes a matrix that describes the transform.

See !TMAT
See *TRANSFORM (type)
See *MAT (type)")
  
  (function tfrom-mat
    "Computes a transform from the affine transformation matrix.

See !TFROM-MAT
See *TRANSFORM (type)
See *MAT (type)")
  
  (function tmove
    "Applies a translation by the given vector to the transform.

The translation is in the local reference frame of the transform.

See *VEC3 (type)
See *TRANSFORM (type)
See TMOVE-BY
See TOFFSET")
  
  (function tmove-by
    "Applies a translation by the given vector to the transform.

The translation is in the local reference frame of the transform.

See *VEC3 (type)
See *TRANSFORM (type)
See TMOVE
See TOFFSET")
  
  (function toffset
    "Applies a translation by the given vector to the transform.

The translation is in the global reference frame.

See *VEC3 (type)
See *TRANSFORM (type)
See TMOVE
See TOFFSET-BY")
  
  (function toffset-by
    "Applies a translation by the given vector to the transform.

The translation is in the global reference frame.

See *VEC3 (type)
See *TRANSFORM (type)
See TMOVE
See TOFFSET")
  
  (function tscale
    "Applies a scaling of the transform by the given vector.

See *VEC3 (type)
See *TRANSFORM (type)
See TSCALE-BY")
  
  (function tscale-by
    "Applies a scaling of the transform by the given vector.

See *VEC3 (type)
See *TRANSFORM (type)
See TSCALE")
  
  (function trotate
    "Applies a rotation of the transform by the given quaternion.

See *QUAT (type)
See *TRANSFORM (type)
See TROTATE-BY")
  
  (function trotate-by
    "Applies a rotation of the transform around the given axis and angle.

See *VEC3 (type)
See *TRANSFORM (type)
See TROTATE")
  
  (function tx
    "Computes a vector describing the X axis direction of the transform.

See *VEC3 (type)
See *TRANSFORM (type)")
  
  (function ty
    "Computes a vector describing the Y axis direction of the transform.

See *VEC3 (type)
See *TRANSFORM (type)")
  
  (function tz
    "Computes a vector describing the Z axis direction of the transform.

See *VEC3 (type)
See *TRANSFORM (type)")
  
  (function tquat2
    "Computes a dual-quaternion describing the transform.

Note that the dual-quaternion ignores scaling.

See *TRANSFORM (type)
See *QUAT2 (type)")
  
  (function tfrom-quat2
    "Computes a transform from the dual-quaternion.

Note that the scaling of the transform is set to 1,1,1.

See *TRANSFORM (type)
See *QUAT2 (type)"))
