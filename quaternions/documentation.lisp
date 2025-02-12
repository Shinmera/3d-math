#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.quaternions)

(docs:define-docs
  (type quat
    "A single-float quaternion.

See *QUAT (type)
See QX
See QY
See QZ
See QW
See QI
See QJ
See QK
See QR")
  
  (type dquat
    "A double-float quaternion.

See *QUAT (type)
See QX
See QY
See QZ
See QW
See QI
See QJ
See QK
See QR")
  
  (type *quat
    "Type encapsulating all quaternions of any element type.

See QUAT (type)
See DQUAT (type)")
  
  (function qx
    "Accesses the X/i component of the quaternion.

See *QUAT (type)")
  
  (function qy
    "Accesses the Y/j component of the quaternion.

See *QUAT (type)")
  
  (function qz
    "Accesses the Z/k component of the quaternion.

See *QUAT (type)")
  
  (function qw
    "Accesses the W/r component of the quaternion.

See *QUAT (type)")
  
  (function qi
    "Accesses the X/i component of the quaternion.

See *QUAT (type)")
  
  (function qj
    "Accesses the Y/j component of the quaternion.

See *QUAT (type)")
  
  (function qk
    "Accesses the Z/k component of the quaternion.

See *QUAT (type)")
  
  (function qr
    "Accesses the W/r component of the quaternion.

See *QUAT (type)")
  
  (function quat
    "Construct a single-float quaternion.

You may initialise the quaternion as follows:
- No arguments initialises the quaternion with 1+0i+0j+0k
- A VEC3 initialises the quaternion with 1+Xi+Yj+Zk
- A VEC3 and real initialises the quaternion with r+Xi+Yj+Zk
- Three reals initialises the quaternion with 1+Ai+Bj+Ck
- Four reals initialises the quaternion with D+Ai+Bj+Ck
- Another quaternion copies the elements over

See QUAT (type)")
  
  (function dquat
    "Construct a double-float quaternion.

You may initialise the quaternion as follows:
- No arguments initialises the quaternion with 1+0i+0j+0k
- A VEC3 initialises the quaternion with 1+Xi+Yj+Zk
- A VEC3 and real initialises the quaternion with r+Xi+Yj+Zk
- Three reals initialises the quaternion with 1+Ai+Bj+Ck
- Four reals initialises the quaternion with D+Ai+Bj+Ck
- Another quaternion copies the elements over

See QUAT (type)")
  
  (function qcopy
    "Creates a copy of the quaternion.

See *QUAT (type)")
  
  (function qzero
    "Creates a copy of the quaternion with all entries set to zero.

See *QUAT (type)"))

(docs:define-docs
  (function q<-
    "Transfers the elements from the right-hand quaternion to the left-hand quaternion.

See *QUAT (type)")
  
  (function q=
    "Checks whether the passed quaternions are identical in all elements.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See *QUAT (type)
See Q/=
See Q~=
See QEQUAL")
  
  (function q~=
    "Checks whether the passed quaternions are equal in all elements.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See ~=
See *QUAT (type)
See Q/=
See QEQUAL
See Q=")

  (function qequal
    "Checks whether the passed quaternions are equal.

Equal here means that the rotation gives the same end result, but may
differ in the direction in which the rotation happens. Meaning mirror
symmetric rotations are considered equal.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See ~=
See *QUAT (type)
See Q/=
See Q=")
  
  (function q/=
    "Checks whether the passed quaternions are different in any element.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See *QUAT (type)
See Q=
See Q~=")
  
  (function q<
    "Checks whether the passed quaternions are in strictly ascending order.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See *QUAT (type)
See Q<=")
  
  (function q<=
    "Checks whether the passed quaternions are in ascending order.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See *QUAT (type)
See Q<")
  
  (function q>
    "Checks whether the passed quaternions are in strictly descending order.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See *QUAT (type)
See Q>=")
  
  (function q>=
    "Checks whether the passed quaternions are in descending order.

The elements are checked \"in parallel\", meaning the R element
of each quaternion is only checked against the R element of others, and so
forth.

You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See *QUAT (type)
See Q>")
  
  (function q+
    "Adds the elements of each of the specified quaternions up and returns a fresh quaternion with the result.

The elements are added \"in parallel\", meaning the R element
of each matrix is only added to the R element of others, and so
forth.

The quaternions must match in element type.
You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See NQ+
See !Q+
See *QUAT (type)")
  
  (function q-
    "Subtracts the elements of each of the specified quaternions up and returns a fresh quaternion with the result.

The elements are subtracted \"in parallel\", meaning the R element
of each matrix is only added to the R element of others, and so
forth.

The quaternions must match in element type.
You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See NQ-
See !Q-
See *QUAT (type)")
  
  (function q*
    "Multiplies the specified quaternions together and returns a fresh object with the result.

If a VEC3 is passed, the vector is rotated by the quaternion and the
result will be a fresh VEC3.

If a quaternion is passed, the rotations are combined in a
right-to-left order.

The quaternions must match in element type.
You may also pass a REAL in place of a quaternion, in which case the
resulting quaternion is simply multiplied element-wise with the REAL.

See NQ+
See !Q+
See *QUAT (type)")
  
  (function q/
    "Divides the elements of each of the specified quaternions up and returns a fresh quaternion with the result.

The elements are divided \"in parallel\", meaning the R element
of each matrix is only added to the R element of others, and so
forth.

The quaternions must match in element type.
You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See NQ/
See !Q/
See *QUAT (type)")
  
  (function qmin
    "Minimizes the elements of each of the specified quaternions and returns a fresh quaternion with the result.

The elements are compared \"in parallel\", meaning the R element
of each matrix is only added to the R element of others, and so
forth.

The quaternions must match in element type.
You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See NQMIN
See !QMIN
See *QUAT (type)")
  
  (function qmax
    "Maximizes the elements of each of the specified quaternions and returns a fresh quaternion with the result.

The elements are compared \"in parallel\", meaning the R element
of each matrix is only added to the R element of others, and so
forth.

The quaternions must match in element type.
You may also pass a REAL in place of a quaternion, in which case the REAL
is treated as if it were a quaternion with the same value in all
elements.

See NQMAX
See !QMAX
See *QUAT (type)")
  
  (function q.
    "Computes the dot product of the two quaternions.

See *QUAT (type)")
  
  (function qsqrlength
    "Computes the squared length or magnitude of the quaternion.

See *QUAT (type)")
  
  (function qangle
    "Accesses the angle of the quaternion.

See *QUAT (type)")
  
  (function qsetf
    "Updates all elements of the quaternion.

The elements are coerced to the element-type required by the
quaternion.

See *QUAT (type)")

  (function qdistance
    "Returns the angular distance between two quaternions.

See *QUAT (type)")
  
  (function qconjugate
    "Computes the conjugate of the quaternion.

See !QCONJUGATE
See NQCONJUGATE
See *QUAT (type)")
  
  (function qinv
    "Computes the inverses of the quaternion.

See !QINV
See NQINV
See *QUAT (type)")
  
  (function qfrom-angle
    "Computes a quaternion from an axis+angle description

See !QFROM-ANGLE
See *QUAT (type)")
  
  (function qtowards
    "Computes a quaternion describing the rotation from one to another.

See !QTOWARDS
See *QUAT (type)")
  
  (function qlookat
    "Computes a quaternion from a view direction and upwards vector.

See !QLOOKAT
See *QUAT (type)")
  
  (function qexpt
    "Computes the exponentiated quaternion.

See !QEXPT
See NQEXPT
See *QUAT (type)")
  
  (function qunit
    "Computes the unit quaternion.

See !QUNIT
See NQUNIT
See *QUAT (type)")
  
  (function qunit*
    "Computes the unit quaternion.

Unlike QUNIT this will not fail if the quaternion has a length of
zero.

See !QUNIT*
See NQUNIT*
See *QUAT (type)")
  
  (function qmix
    "Computes a mix between the two quaternions.

Note that this mix is done linearly and may thus give undesired
results for large differences in the arguments.

See !QMIX
See NQMIX
See *QUAT (type)")
  
  (function qnlerp
    "Computes the linear interpolation of the two quaternions.

See !QNLERP
See NQNLERP
See *QUAT (type)")
  
  (function qslerp
    "Computes the spherical interpolation of the two quaternions.

This will rotate along the unit sphere, preserving the rotation even
under large differences in the arguments.

See !QSLERP
See NQSLERP
See *QUAT (type)")

  (function q+*
    "Perform a scaled addition without an intermediate product.

See Q+
See Q*
See *QUAT (type)")

  (function qrand
    "Returns a random unit quaternion.

See QUAT (type)")
  
  (function qmat
    "Constructs an affine transformation matrix describing the quaternion's rotation.

The matrix may either be 3x3 or 4x4.

See !QMAT
See *QUAT (type)")
  
  (function qfrom-mat
    "Constructs a quaternion from an affine matrix describing a rotation.

See !QFROM-MAT
See *QUAT (type)")
  
  (function qaxis
    "Accesses the rotation axis of the quaternion as a unit vector.

See *QUAT (type)")
  
  (function qlength
    "Returns the length or magnitude of the quaternion.

See *QUAT (type)")

  (function qeuler
    "Returns the euler angle deconstruction of the quaternion.

See *QUAT (type)
See *VEC (type)"))
