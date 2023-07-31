#|
 This file is a part of 3d-math
 (c) 2023 Shirakumo http://shirakumo.org (shirakumo@tymoon.eu)
|#

(asdf:defsystem 3d-math-test
  :version "3.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-math system."
  :homepage "https://Shinmera.github.io/3d-math/"
  :bug-tracker "https://github.com/Shinmera/3d-math/issues"
  :source-control (:git "https://github.com/Shinmera/3d-math.git")
  :serial T
  :components ((:module "test"
                :components ((:file "vectors")
                             (:file "matrices"))))
  :depends-on (:3d-math :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :3d-math-test)))
