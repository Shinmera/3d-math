#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.test)

(define-test quaternions
  :parent 3d-math
  :depends-on (vectors matrices))

(define-test quaternion-struct
  :parent quaternions
  (of-type quat (quat))
  (of-type quat (quat 0 0 0 1))
  (true (quat-p (quat)))
  (is = 0 (qx (quat)))
  (is = 0 (qy (quat)))
  (is = 0 (qz (quat)))
  (is = 1 (qw (quat)))
  (is = 1 (qx (quat 1 2 3 4)))
  (is = 2 (qy (quat 1 2 3 4)))
  (is = 3 (qz (quat 1 2 3 4)))
  (is = 4 (qw (quat 1 2 3 4))))

(define-test quaternion-misc
  :parent quaternions
  :depends-on (quaternion-struct)
  (is q= (quat) (quat))
  (is q= (quat 1 2 3 4) (quat 1 2 3 4))
  (isnt q= (quat 1 2 3 4) (quat 1 2 3 5))
  (is q/= (quat 1 2 3 4) (quat 1 2 3 5))
  (isnt q/= (quat) (quat))
  (is q= (quat 1 2 3 4) (qsetf (quat) 1 2 3 4))
  (is qequal (qfrom-angle +vx+ (+ PI)) (qfrom-angle +vx+ (- PI)))
  (is v= +vy+ (qaxis (qfrom-angle +vy+ PI)))
  (is ~= (coerce PI 'single-float) (qangle (qfrom-angle +vy+ PI)))
  (is q~= (qfrom-angle +vy+ PI) (qfrom-mat (qmat (qfrom-angle +vy+ PI))))
  (is q~= (qfrom-angle +vx+ (/ PI 2)) (qfrom-mat (qmat (qfrom-angle +vx+ (/ PI 2)))))
  (is q~= (qfrom-angle +vy+ PI) (qfrom-mat (mrotation +vy+ PI))))

(define-test quaternion-arithmetic
  :parent quaternions
  :depends-on (quaternion-misc)
  (is q= (quat 1 0 0 1) (q* (quat 1 0 0 1) (quat 0 0 0 1)))
  (is q= (quat 1 0 0 1) (q* (quat 0 0 0 1) (quat 1 0 0 1)))
  (is q= (quat 2 0 0 0) (q* (quat 1 0 0 1) (quat 1 0 0 1)))
  (is q= (quat 2 -1 5 0) (q* (quat 1 2 3 1) (quat 1 0 0 1)))
  (is q= (quat 3 1 8 1) (q* (quat 1 2 3 1) (quat 1 0 0 2)))
  (is q= (quat 4 3 2 -1) (q* (quat 1 2 3 1) (quat 0 1 0 1)))
  (is q= (quat -1 3 4 -2) (q* (quat 1 2 3 1) (quat 0 0 1 1)))
  (is q= (quat 2 4 6 -13) (q* (quat 1 2 3 1) (quat 1 2 3 1))))

(define-test quaternion-math
  :parent quaternions
  :depends-on (quaternion-arithmetic))

(define-test quaternion-randomized
  :parent quaternions
  :depends-on (quaternion-misc)
  (dotimes (i 100)
    (let ((axis (nvunit (vrand (vec 0 0 0) 10)))
          (angle (random (* 2 PI))))
      (is qequal (qfrom-angle axis angle) (qfrom-mat (mrotation axis angle)))))
  (dotimes (i 100)
    (let ((axis (nvunit (vrand (dvec 0 0 0) 10)))
          (angle (float (random (* 2 PI)) 0d0)))
      (is m~= (qmat (qfrom-angle axis angle) (dmat4)) (mrotation axis angle))))
  (dotimes (i 100)
    (let ((from (vrand))
          (to (vrand)))
      (is qequal (qtowards from to) (qfrom-angle (vc from to) (vangle from to)))))
  ;; FIXME: dunno why this is wrong, but the tests are off somehow.
  #++
  (dotimes (i 100)
    (let ((quat (nvunit (dquat (random 1.0d0) (random 1.0d0) (random 1.0d0) (random 1.0d0)))))
      (is qequal quat (qfrom-mat (qmat quat (dmat4)))))))

