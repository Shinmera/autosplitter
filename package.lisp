(defpackage #:org.shirakumo.fraf.autosplitter
  (:use #:cl)
  (:nicknames #:autosplitter)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:v #:org.shirakumo.verbose))
  (:export
   #:launch))
