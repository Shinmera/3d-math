#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(asdf:defsystem 3d-math
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library implementing the necessary linear algebra math for 2D and 3D computations"
  :homepage "https://shinmera.github.io/3d-math/"
  :bug-tracker "https://github.com/shinmera/3d-math/issues"
  :source-control (:git "https://github.com/shinmera/3d-math.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               ;; We load the modules interleaved like this in order to allow
               ;; the ops to use other types and so on.
               (:file "vectors/package")
               (:file "matrices/package")
               (:file "quaternions/package")
               (:file "dual-quaternions/package")
               (:file "transforms/package")
               (:file "vectors/types")
               (:file "matrices/types")
               (:file "quaternions/types")
               (:file "dual-quaternions/types")
               (:file "transforms/types")
               (:file "vectors/raw-ops")
               (:file "matrices/raw-ops")
               (:file "quaternions/raw-ops")
               (:file "vectors/ops")
               (:file "matrices/ops")
               (:file "quaternions/ops")
               (:file "dual-quaternions/raw-ops")
               (:file "dual-quaternions/ops")
               (:file "transforms/raw-ops")
               (:file "transforms/ops")
               (:file "vectors/documentation")
               (:file "matrices/documentation")
               (:file "quaternions/documentation")
               (:file "dual-quaternions/documentation")
               (:file "transforms/documentation"))
  :depends-on (:documentation-utils
               :type-templates)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-math-test))))
