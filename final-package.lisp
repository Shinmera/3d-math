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
