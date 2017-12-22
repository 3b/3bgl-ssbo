(defsystem :3bgl-ssbo
  :description "writer for OpenGL SSBO/UBO data"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria glsl-packing trivial-garbage cl-opengl)
  :serial t
  :components ((:file "package")
               (:file "ssbo")
               (:file "walker")
               (:file "ssbo-direct")))
