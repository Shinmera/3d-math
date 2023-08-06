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
               (:module "vectors"
                :components ((:file "package")
                             (:file "types")
                             (:file "raw-ops")
                             (:file "ops")
                             (:file "documentation")))
               (:module "matrices"
                :components ((:file "package")
                             (:file "types")
                             (:file "raw-ops")
                             (:file "ops")
                             (:file "documentation")))
               (:module "quaternions"
                :components ((:file "package")
                             (:file "types")
                             (:file "raw-ops")
                             (:file "ops")
                             (:file "documentation")))
               (:module "dual-quaternions"
                :components ((:file "package")
                             (:file "types")
                             (:file "raw-ops")
                             (:file "ops")
                             (:file "documentation")))
               (:module "transforms"
                :components ((:file "package")
                             (:file "types")
                             (:file "raw-ops")
                             (:file "ops")
                             (:file "documentation"))))
  :depends-on (:documentation-utils
               :type-templates)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-math-test))))
