(unless (find-package '#:org.shirakumo.fraf.math)
  (make-package '#:org.shirakumo.fraf.math
                :use '(#:org.shirakumo.fraf.math.vectors
                       #:org.shirakumo.fraf.math.matrices
                       #:org.shirakumo.fraf.math.quaternions
                       #:org.shirakumo.fraf.math.transforms
                       #:org.shirakumo.fraf.math.dual-quaternions)))

(export (loop for symbol being the symbols of '#:org.shirakumo.fraf.math
              collect symbol)
        '#:org.shirakumo.fraf.math)
