#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(in-package #:org.shirakumo.fraf.math.internal)

(export (loop for symbol being the symbols of '#:org.shirakumo.fraf.math
              collect symbol)
        '#:org.shirakumo.fraf.math)

(loop for symbol being the symbols of '#:org.shirakumo.fraf.math
      do (when (and (fboundp symbol) (documentation symbol 'function))
           (let ((! (find-symbol (format NIL "!~a" symbol) '#:org.shirakumo.fraf.math))
                 (n (find-symbol (format NIL "~a~a" :n symbol) '#:org.shirakumo.fraf.math)))
             (when (and ! (not (documentation ! 'function)))
               (setf (documentation ! 'function)
                     (format NIL "Transferring variant of ~a~%~%See ~a" symbol symbol)))
             (when (and n (not (documentation n 'function)))
               (setf (documentation n 'function)
                     (format NIL "Modifying variant of ~a~%~%See ~a" symbol symbol))))))

(in-package #:org.shirakumo.fraf.math)
(org.shirakumo.type-templates:define-templated-dispatch org.shirakumo.fraf.math.internal:*zero (x)
  :ignore-template-types (org.shirakumo.fraf.math.internal:vec-type
                          org.shirakumo.fraf.math.internal:mat-type
                          org.shirakumo.fraf.math.internal:quat-type
                          org.shirakumo.fraf.math.internal:quat2-type
                          org.shirakumo.fraf.math.internal:transform-type)
  ((org.shirakumo.fraf.math.internal:vec-type) (vzero) x)
  ((org.shirakumo.fraf.math.internal:mat-type) (mzero) x)
  ((org.shirakumo.fraf.math.internal:quat-type) (qzero) x)
  ((org.shirakumo.fraf.math.internal:quat2-type) (q2zero) x)
  ((org.shirakumo.fraf.math.internal:transform-type) (tzero) x))
