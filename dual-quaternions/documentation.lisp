#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.dual-quaternions)

(docs:define-docs
  (type quat2
    "A single-float dual-quaternion.

See *QUAT2 (type)
See QUAT2
See Q2REAL
See Q2DUAL")
  
  (type dquat2
    "A double-float dual-quaternion.

See *QUAT2 (type)
See DQUAT2
See Q2REAL
See Q2DUAL")
  
  (type *quat2
    "Type encapsulating all dual-quaternions of any element type.

See QUAT2 (type)
See DQUAT2 (type)")
  
  (function q2real
    "Accesses the real component of the dual-quaternion.

See *QUAT2 (type)
See *QUAT (type)")
  
  (function q2dual
    "Accesses the dual component of the dual-quaternion.

See *QUAT2 (type)
See *QUAT (type)")
  
  (function quat2
    "Construct a single-float dual-quaternion

You may initialise the dual-quaternion as follows:
- No arguments construct a dual-quaternion of [0,0,0,1][0,0,0,0]
- A QUAT supplies the REAL part, with the DUAL [0,0,0,0]
- Two QUATs supply the REAL and DUAL parts respectively
- Another DUAL-QUATERNION creates a copy

See QUAT2 (type)")
  
  (function dquat2
    "Construct a double-float dual-quaternion

You may initialise the dual-quaternion as follows:
- No arguments construct a dual-quaternion of [0,0,0,1][0,0,0,0]
- A QUAT supplies the REAL part, with the DUAL [0,0,0,0]
- Two QUATs supply the REAL and DUAL parts respectively
- Another DUAL-QUATERNION creates a copy

See DQUAT2 (type)")
  
  (function q2copy
    "Creates a copy of the dual-quaternion.

The contained quaternions are not shared with the resulting
dual-quaternion.

See *QUAT2 (type)")
  
  (function q2zero
    "Creates a copy of the dual-quaternion initialised to [0,0,0,1][0,0,0,0]

The contained quaternions are not shared with the resulting
dual-quaternion.

See *QUAT2 (type)"))

(docs:define-docs
  (function q2+
    "Adds the dual-quaternions element-wise.

This is like invoking Q+ on the REAL and DUAL parts separately.

See !Q2+
See NQ2+
See *QUAT2 (type)")
  
  (function q2-
    "Subtracts the dual-quaternions element-wise.

This is like invoking Q- on the REAL and DUAL parts separately.

See !Q2-
See NQ2-
See *QUAT2 (type)")
  
  (function q2*
    "Multiplies the dual-quaternions.

If an argument is a VEC3, the vector is rotated instead and the
resulting rotated vector is returned.
If an argument is a QUAT, the quaternion is multiplied instead and the
resulting multiplied quaternion is returned.

See !Q2*
See NQ2*
See *QUAT2 (type)")
  
  (function q2/
    "Divides the dual-quaternions element-wise.

This is like invoking Q/ on the REAL and DUAL parts separately.

See !Q2/
See NQ2/
See *QUAT2 (type)")
  
  (function q2=
    "Checks whether the passed dual-quaternions are identical in all elements.

The elements are checked \"in parallel\", meaning the REAL element
of each quaternion is only checked against the REAL element of others,
and the same for the DUAL element.

See Q2~=
See Q2/=
See *QUAT2 (type)")
  
  (function q2~=
    "Checks whether the passed dual-quaternions are equal in all elements.

The elements are checked \"in parallel\", meaning the REAL element
of each quaternion is only checked against the REAL element of others,
and the same for the DUAL element.

See Q2=
See Q2/=
See *QUAT2 (type)")
  
  (function q2/=
    "Checks whether the passed dual-quaternions are different in any element.

The elements are checked \"in parallel\", meaning the REAL element
of each quaternion is only checked against the REAL element of others,
and the same for the DUAL element.

See Q2=
See Q2~=
See *QUAT2 (type)")
  
  (function q2conjugate
    "Computes the conjugate of the dual-quaternion.

See !Q2CONJUGATE
See NQ2CONJUGATE
See *QUAT2 (type)")
  
  (function q2unit
    "Computes the unit dual-quaternion.

See !Q2UNIT
See NQ2UNIT
See *QUAT2 (type)")
  
  (function q2unit*
    "Computes the unit dual-quaternion.

Unlike Q2UNIT this will not fail if the dual-quaternion has a length of
zero.

See !Q2UNIT*
See NQ2UNIT*
See *QUAT2 (type)")
  
  (function q2location
    "Returns the location or position of the dual-quaternion.

See !Q2LOCATION
See *QUAT2 (type)")
  
  (function q2from-location
    "Computes a dual-quaternion from a quaternion and location or position.

See !Q2FROM-LOCATION
See *QUAT2 (type)")
  
  (function q2.
    "Returns the dot product of the real part of the dual-quaternions.

See *QUAT2 (type)")
  
  (function q2sqrlength
    "Returns the squared length or magnitude of the real part of the dual-quaternion.

See *QUAT2 (type)")
  
  (function q2length
    "Returns the length or magnitude of the real part of the dual-quaternion.

See *QUAT2 (type)"))
