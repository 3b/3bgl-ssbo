(defsystem :3bgl-ssbo
  :description "writer for OpenGL SSBO/UBO data"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria glsl-packing)
  :serial t
  :components ((:file "package")
               (:file "ssbo")))
