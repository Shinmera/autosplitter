(asdf:defsystem autosplitter
  :version "0.2.0"
  :build-operation "deploy-op"
  :build-pathname #+linux "autosplitter-linux.run"
  #+darwin "autosplitter-macos.o"
  #+win32 "autosplitter-windows"
  #+(and bsd (not darwin)) "autosplitter-bsd.run"
  #-(or linux bsd win32) "autosplitter"
  :entry-point "org.shirakumo.fraf.autosplitter::main"
  :components ((:file "package")
               (:file "stats")
               (:file "save")
               (:file "ui"))
  :serial T
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :trial-alloy
               :lquery))
