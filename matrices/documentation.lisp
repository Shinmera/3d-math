#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.matrices)

(docs:define-docs
  (type mat2
    "A 2x2 single-float matrix.

See MAT2
See MARR2")
  
  (type mat3
    "A 3x3 single-float matrix.

See MAT3
See MARR3")
  
  (type mat4
    "A 4x4 single-float matrix.

See MAT4
See MARR4")
  
  (type matn
    "A mxn single-float matrix.

See MATN
See MARRN")
  
  (type mat
    "")
  
  (type fmat
    "Type encapsulating single-float matrices of any size.

See MAT (type)
See MAT2 (type)
See MAT3 (type)
See MAT4 (type)
See MATN (type)")
  
  (type dmat
    "Type encapsulating double-float matrices of any size.

See DMAT (type)
See DMAT2 (type)
See DMAT3 (type)
See DMAT4 (type)
See DMATN (type)")
  
  (type imat
    "Type encapsulating signed-byte 32 matrices of any size.

See IMAT (type)
See IMAT2 (type)
See IMAT3 (type)
See IMAT4 (type)
See IMATN (type)")
  
  (type umat
    "Type encapsulating unsigned-byte 32 matrices of any size.

See UMAT (type)
See UMAT2 (type)
See UMAT3 (type)
See UMAT4 (type)
See UMATN (type)")
  
  (type *mat2
    "Type encapsulating 2x2 matrices of any element type.

See MAT2 (type)
See DMAT2 (type)
See IMAT2 (type)
See UMAT2 (type)")
  
  (type *mat3
    "Type encapsulating 3x3 matrices of any element type.

See MAT3 (type)
See DMAT3 (type)
See IMAT3 (type)
See UMAT3 (type)")
  
  (type *mat4
    "Type encapsulating 4x4 matrices of any element type.

See MAT4 (type)
See DMAT4 (type)
See IMAT4 (type)
See UMAT4 (type)")
  
  (type *matn
    "Type encapsulating mxn matrices of any element type.

See MATN (type)
See DMATN (type)
See IMATN (type)
See UMATN (type)")
  
  (type *mat
    "Type encapsulating matrices of any element type and size.

See MAT2 (type)
See DMAT2 (type)
See IMAT2 (type)
See UMAT2 (type)
See MAT3 (type)
See DMAT3 (type)
See IMAT3 (type)
See UMAT3 (type)
See MAT4 (type)
See DMAT4 (type)
See IMAT4 (type)
See UMAT4 (type)
See MATN (type)
See DMATN (type)
See IMATN (type)
See UMATN (type)")
  
  (function mat2
    "Construct a 2x2 single-float matrix.

You may initialise the matrix as follows:
- No arguments initialises all entries to zero
- One argument initialises all entries to that value
- 4 arguments initialise the entries to those values
- A 4-element sequence copies the elements in row-major order
- 2 VEC2s will be used as the columns of the matrix
- A *MAT2 will coerce and transfer the entries

See MAT
See MAT2 (type)")
  
  (function mat3
    "Construct a 3x3 single-float matrix.

You may initialise the matrix as follows:
- No arguments initialises all entries to zero
- One argument initialises all entries to that value
- 9 arguments initialise the entries to those values
- A 9-element sequence copies the elements in row-major order
- 3 VEC3s will be used as the columns of the matrix
- A *MAT3 will coerce and transfer the entries

See MAT
See MAT3 (type)")
  
  (function mat4
    "Construct a 4x4 single-float matrix.

You may initialise the matrix as follows:
- No arguments initialises all entries to zero
- One argument initialises all entries to that value
- 16 arguments initialise the entries to those values
- A 16-element sequence copies the elements in row-major order
- 4 VEC4s will be used as the columns of the matrix
- A *MAT4 will coerce and transfer the entries

See MAT
See MAT4 (type)")
  
  (function matn
    "Construct a mxn single-float matrix.

You must pass the number of columns and rows of the matrix first, then
the elements.

See MAT
See MATN (type)")
  
  (function mat
    "Construct a single-float matrix.

You may initialise the matrix as follows:
- One argument creates a square zero matrix of that size
- Two arguments creates a mxn zero matrix of that size
- Three arguments creates a mxn zero matrix of that size with elements
  filled from the sequence in the third argument.
- 4 arguments creates a 2x2 matrix with those elements
- 9 arguments creates a 3x3 matrix with those elements
- 16 arguments creates a 4x4 matrix with those elements
- A 4-element sequence creates a 2x2 matrix with the elements in
  row-major order
- A 9-element sequence creates a 3x3 matrix with the elements in
  row-major order
- A 16-element sequence creates a 4x4 matrix with the elements in
  row-major order
- 2 VEC2s will be used as the columns of a 2x2 matrix
- 3 VEC3s will be used as the columns of a 3x3 matrix
- 4 VEC4s will be used as the columns of a 4x4 matrix
- A *MAT will coerce and transfer the entries to a matrix of the same
  size.

See MAT2 (type)
See MAT3 (type)
See MAT4 (type)
See MATN (type)")
    
  (function marr
    "Accesses the element array of the matrix.

See *MAT")
  
  (function mcols
    "Returns the number of columns of the matrix.

See *MAT")
  
  (function mrows
    "Returns the number of rows of the matrix.

See *MAT")
  
  (function write-matrix
    "Writes the matrix to a stream in the given format.

The following formats are possible:
  :NICE     --- Presents a human-readable view of the matrix
  :WOLFRAM  --- Uses the Wolfram-compatible syntax
  :ARRAY    --- Uses the Common Lisp 2D-array syntax
  :C        --- Uses the C row-major array syntax
  :JSON     --- Uses the JSON row-major array syntax

STREAM may be either one of:
  NIL --- The output is returned as a string
  T   --- The output is written to *STANDARD-OUTPUT*
  A STREAM

See *MAT"))

(docs:define-docs
  (function m=
    "Checks whether the passed matrices are identical in all elements.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M/=
See M~=")
  
  (function m~=
    "Checks whether the passed matrices are equal in all elements.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M/=
See M=
See ~=")
  
  (function m/=
    "Checks whether the passed matrices are different in any of the elements.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M=
See M~=")
  
  (function m<
    "Checks whether the passed matrices are in strictly ascending order.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M<=")
  
  (function m<=
    "Checks whether the passed matrices are in ascending order.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M<")
  
  (function m>
    "Checks whether the passed matrices are in strictly descending order.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M>=")
  
  (function m>=
    "Checks whether the passed matrices are in descending order.

The elements are checked \"in parallel\", meaning the 0,0 element
of each matrix is only checked against the 0,0 element of others, and so
forth.

The matrices must match in arity.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See *MAT (type)
See M>")

  (function m<-
    "Copy the elements from the right hand matrix to the left hand matrix.

The matrices must match in arity.

The updated matrix is returned.

See *MAT (type)")
  
  (function mapply
    "Applies the function to each element of the matrix.

Returns a fresh matrix with the modified elements.

See NMAPPLY
See *MAT (type)")
  
  (function mcof
    "Computes the cofactor matrix.

See NMCOF
See !MCOF
See *MAT (type)")
  
  (function minv
    "Computes the matrix inverses.

See MINV-AFFINE
See NMINV
See !MINV
See *MAT (type)")
  
  (function minv-affine
    "Computes the matrix inverses for special-case affine transforms.

This only works as expected if the matrix encapsulates a rotation and
a translation, it will not correctly invert arbitrary square
matrices.

See NMINV-AFFINE
See !MINV-AFFINE
See MINV
See *MAT (type)")
  
  (function mtranspose
    "Computes the transpose of the matrix.

Returns a fresh matrix with the transposed elements.

See NMTRANSPOSE
See !MTRANSPOSE
See *MAT (type)")
  
  (function mswap-row
    "Swaps two rows of the matrix.

Returns a fresh matrix with the rows swapped.

See NMSWAP-ROW
See !MSWAP-ROW
See *MAT (type)")
  
  (function mswap-col
    "Swaps two columns of the matrix.

Returns a fresh matrix with the columns swapped.

See NMSWAP-COL
See !MSWAP-COL
See *MAT (type)")
  
  (function mrow
    "Returns a particular row of the matrix as a vector.

See !MROW
See *MAT (type)")
  
  (function mcol
    "Returns a particular column of the matrix as a vector.

See !MCOL
See *MAT (type)")
  
  (function mdiag
    "Returns the diagonal of the matrix as a vector.

See !MDIAG
See *MAT (type)")
  
  (function mminor
    "Computes the given minor matrix.

See *MAT (type)")
  
  (function mdet
    "Computes the determinant of the matrix.

See *MAT (type)")
  
  (function mtrace
    "Computes the trace of the matrix.

See *MAT (type)")
  
  (function m1norm
    "Computes the 1/column norm of the matrix.

See *MAT (type)")
  
  (function minorm
    "Computes the infinity/max norm of the matrix.

See *MAT (type)")
  
  (function m2norm
    "Computes the 2/Frobenius/Hilbert-Schmidt norm of the matrix.

See *MAT (type)")
  
  (function m+
    "Adds the elements of each of the specified matrices up and returns a fresh matrix with the result.

The elements are added \"in parallel\", meaning the 0,0 element
of each matrix is only added to the 0,0 element of others, and so
forth.

The matrices must match in arity and element type.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See NM+
See !M+
See *MAT (type)")
  
  (function m-
    "Subtracts the elements of each of the specified matrices up and returns a fresh matrix with the result.

The elements are subtracted \"in parallel\", meaning the 0,0 element
of each matrix is only added to the 0,0 element of others, and so
forth.

The matrices must match in arity and element type.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

If only one matrix is passed, all elements are negated.

See NM-
See !M-
See *MAT (type)")
  
  (function m*
    "Multiplies the specified matrices up and returns a fresh value with the result.

The matrices must be compatible in dimensions according to matrix
multiplication rules and match in the element type. Matrices are
multiplied from right to left.

You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

You may also pass a VEC in place of a matrix, in which case the vector
is treated as a column-matrix, and the result is a vector. As a
special convenience you may also multiply larger square matrices with
smaller vectors, as in MAT4*VEC3 is valid. In this case the missing
elements of the vector are treated as if they were 1. Meaning

  (m* (mat4) (vec3))

is equivalent to

  (vxyz (m* (mat4) (vec (vec3) 1)))

Just with less overhead.

See NM*
See !M*
See *MAT (type)
See *VEC (type)")
  
  (function m/
    "Divides the elements of each of the specified matrices up and returns a fresh matrix with the result.

The elements are divided \"in parallel\", meaning the 0,0 element
of each matrix is only added to the 0,0 element of others, and so
forth.

The matrices must match in arity and element type.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

If only one matrix is passed, all elements are inverted.

See NM/
See !M/
See *MAT (type)")
  
  (function mmin
    "Computes the element-wise minimum of the passed matrices and returns a fresh matrix with the result.

The elements are compared \"in parallel\", meaning the 0,0 element
of each matrix is only added to the 0,0 element of others, and so
forth.

The matrices must match in arity and element type.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See NMMIN
See !MMIN
See *MAT (type)")
  
  (function mmax
    "Computes the element-wise maximum of the passed matrices and returns a fresh matrix with the result.

The elements are compared \"in parallel\", meaning the 0,0 element
of each matrix is only added to the 0,0 element of others, and so
forth.

The matrices must match in arity and element type.
You may also pass a REAL in place of a matrix, in which case the REAL
is treated as if it were a matrix with the same value in all
elements.

See NMMAX
See !MMAX
See *MAT (type)")
  
  (function n*m
    "Same as NM* but modifying the right-hand side instead.

See NM*
See M*
See *MAT (type)")
  
  (function nmtranslate
    "Performs a translation on the matrix.

This is analogous to (NM* m (MTRANSLATION v))

See MTRANSLATION
See NM*
See *MAT (type)")
  
  (function nmscale
    "Performs a scaling of the matrix.

This is analogous to (NM* m (MSCALING v))

See MSCALING
See NM*
See *MAT (type)")
  
  (function nmrotate
    "Performs a rotation on the matrix.

This is analogous to (NM* m (MROTATION v))

See MROTATION
See NM*
See *MAT (type)")
  
  (function mtranslation
    "Creates a matrix that encapsulates an affine translation.

See NMTRANSLATION
See *MAT (type)")
  
  (function mscaling
    "Creates a matrix that encapsulates an affine scaling.

See NMSCALING
See *MAT (type)")
  
  (function mrotation
    "Creates a matrix that encapsulates an affine rotation around the given axis.

See NMROTATION
See *MAT (type)")
  
  (function mlookat
    "Creates a matrix that performs a translation and rotation to \"look at\" the given target.

See NMLOOKAT
See *MAT (type)")
  
  (function mfrustum
    "Creates a matrix that provides a frustum view box projection.

See NMFRUSTUM
See *MAT (type)")
  
  (function mperspective
    "Creates a matrix that provides a perspective skewed projection.

See NMPERSPECTIVE
See *MAT (type)")
  
  (function mortho
    "Creates a matrix that provides an orthographic projection.

See NMORTHO
See *MAT (type)")

  (function mtensor
    "Computes the tensor product of the two vectors.

See !MTENSOR
See *MAT (type)")

  (function m*4/3
    "Perform a multiplication of a mat4 by a vec3 using only the upper 3x3 block.

This is useful for affine transformation matrices when you want to
eliminate translation and only apply scaling and rotation.

See !M*4/3
See N*M4/3
See M*4/3INV")

  (function m*4/3inv
    "Perform a multiplication of an inverted mat4 by a vec3 using only the upper 3x3 block.

This is useful for affine transformation matrices when you want to
eliminate translation and only apply the inverse scaling and
rotation.

See !M*4/3INV
See N*M4/3INV
See M*4/3")
  
  (function meye
    "Creates a square identity matrix of the requested size.

You may pass either a number specifying the size, or another matrix to
use as the specification instead.

See !MEYE
See *MAT (type)")
  
  (function mrand
    "Creates a square matrix of the requested size with all entries randomised.

You may pass either a number specifying the size, or another matrix to
use as the specification instead.

See !MRAND
See *MAT (type)")
  
  (function mzero
    "Creates a matrix of the requested size with all entries zero.

You may pass either a number specifying the size, or another matrix to
use as the specification instead.

See !MZERO
See *MAT (type)")
  
  (function mpivot
    "Computes a partial pivoting matrix.

Returns three values:
  1. The pivotised matrix
  2. The permutation matrix
  3. The number of permutations performed

See *MAT (type)")
  
  (function mlu
    "Computes the LU factorisation of the matrix.

Returns three values:
  1. The combined LU matrix
  2. The permutation matrix
  3. The number of permutations performed

Se *MAT (type)")
  
  (function mqr
    "Computes the QR factorisation of the matrix.

Returns two values:
  1. The Q matrix
  2. The R matrix

See *MAT (type)")
  
  (function meigen
    "Computes the eigenvalues of the matrix.

Returns the eigenvalues as a vector.

The eigenvalues are computed via iterative QR factorisation. Higher
iterations should yield more accurate results.

See MQR
See *MAT (type)")
  
  (function mcofactor
    "Computes the cofactor at the specified index of the matrix.

See MMINOR
See *MAT (type)")
  
  (function madj
    "Computes the adjugate of the matrix.

See *MAT (type)")
  
  (function mcref
    "Accesses an element of the matrix using COLUMN, ROW indices.

See *MAT (type)")
  
  (function miref
    "Accesses an element of the matrix using a row-major index.

See *MAT (type)")
  
  (function mtransfer
    "Transfers a sub-block of the right-hand matrix to the left-hand matrix.

W is the number of columns to transfer.
H is the number of rows to transfer.
XX is the starting row index in the left-hand matrix.
XY is the starting column index in the left-hand matrix.
MX is the starting row index in the right-hand matrix.
MY is the starting column index in the right-hand matrix.

The matrices must match in their element-type.

See MBLOCK
See *MAT (type)")
  
  (function mblock
    "Returns a sub-block of the matrix as a fresh matrix.

See MTRANSFER
See *MAT (type)")
  
  (function msetf
    "Sets the elements of the matrix to the given values.

Returns the modified matrix.

The values are coerced to the element-type of the matrix.

See *MAT (type)")
  
  (function with-fast-matref
    "Provides convenient access to matrix elements.

ACCESSOR will be a local accessor that takes two arguments:
  The row and column of the element to access

The body will be emitted inside an etypecase that matches the matrix
type specifically, to allow for the best performance.

See MARR
See *MAT (type)"))
