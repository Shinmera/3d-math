(asdf:load-system :staple-markless)

(defmethod staple:definition-wanted-p ((_ definitions:setf-expander) page) NIL)
#+sbcl(defmethod staple:definition-wanted-p ((_ definitions:source-transform) page) NIL)
#+sbcl(defmethod staple:definition-wanted-p ((_ definitions:transform) page) NIL)
#+sbcl(defmethod staple:definition-wanted-p ((_ definitions:vop) page) NIL)
#+sbcl(defmethod staple:definition-wanted-p ((_ definitions:ir1-convert) page) NIL)

(defmethod staple:packages ((system (eql (asdf:find-system "3d-math"))))
  (list (find-package '#:org.shirakumo.fraf.math)))
