#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.quaternions)

(define-alias qaxis (q)
  `(vunit ,q))

(define-alias (setf qaxis) (value q)
  (let ((v (gensym "VALUE")))
    `(let ((,v ,value))
       (v<- ,q ,v)
       ,v)))

(define-alias qsqrlength (q)
  `(q. ,q ,q))

(define-alias qlength (q)
  `(sqrt (qsqrlength ,q)))
