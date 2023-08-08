#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.test)

(define-test transforms
  :parent 3d-math)

(define-test transform-struct
  :parent transforms
  (of-type transform (transform))
  (of-type transform (transform (vec 1 2 3) (vec 4 5 6) (quat 7 8 9 0)))
  (true (transform-p (transform)))
  (is v= (vec 0 0 0) (tlocation (transform)))
  (is v= (vec 1 1 1) (tscaling (transform)))
  (is q= (quat 0 0 0 1) (trotation (transform))))

(define-test transform-arithmetic
  :parent transforms
  :depends-on (transform-struct)
  (is t~= (transform) (t+ (transform (vec 1 2 3)) (transform (vec -1 -2 -3))))
  (is t~= (transform) (t+ (transform (vec 0 0 0) (vec 2 2 2)) (transform (vec 0 0 0) (vec 0.5 0.5 0.5))))
  (is t~= (transform) (t+ (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI)) (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ (- PI)))))
  (is t~= (transform) (t+ (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI)) (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI))))
  (is t~= (transform) (t+ (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI)) (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI)))))

(define-test transform-translation
  :parent transforms
  :depends-on (transform-struct)
  (is m~= (meye 4) (tmat (transform)))
  (is m~= (mtranslation (vec 1 2 3)) (tmat (transform (vec 1 2 3))))
  (is m~= (mscaling (vec 1 2 3)) (tmat (transform (vec 0 0 0) (vec 1 2 3))))
  (is m~= (mrotation +vx+ PI) (tmat (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI))))
  (is t~= (transform) (tfrom-mat (meye 4)))
  (is t~= (transform (vec 1 2 3)) (tfrom-mat (mtranslation (vec 1 2 3))))
  (is t~= (transform (vec 0 0 0) (vec 1 2 3)) (tfrom-mat (mscaling (vec 1 2 3))))
  (is t~= (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI)) (tfrom-mat (mrotation +vx+ PI))))
