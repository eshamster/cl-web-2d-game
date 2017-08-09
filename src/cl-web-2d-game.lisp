(in-package :cl-user)

;; This package is depend on the following js libraries.
;; - three.js
;; - dat.gui.js
;; - threex.keyboardstate.js

;; Note: ":use" not only re-exporting is also requred. Because
;; ps-experiment:with-use-ps-pack solves dependencies using package-use-list.

(defpackage :cl-web-2d-game
  (:use :cl
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.basic-systems
        :cl-web-2d-game.utils
        :cl-web-2d-game.calc
        :cl-web-2d-game.camera
        :cl-web-2d-game.collision
        :cl-web-2d-game.input
        :cl-web-2d-game.texture
        :cl-web-2d-game.2d-geometry
        :cl-web-2d-game.draw-model-system
        :cl-web-2d-game.performance
        :cl-web-2d-game.gui
        :cl-web-2d-game.logger
        :cl-web-2d-game.debug-drawer
        :cl-web-2d-game.initializer))
(in-package :cl-web-2d-game)

(cl-reexport:reexport-from :cl-web-2d-game.basic-components)
(cl-reexport:reexport-from :cl-web-2d-game.basic-systems)
(cl-reexport:reexport-from :cl-web-2d-game.utils)
(cl-reexport:reexport-from :cl-web-2d-game.calc)
(cl-reexport:reexport-from :cl-web-2d-game.camera)
(cl-reexport:reexport-from :cl-web-2d-game.collision)
(cl-reexport:reexport-from :cl-web-2d-game.input)
(cl-reexport:reexport-from :cl-web-2d-game.texture)
(cl-reexport:reexport-from :cl-web-2d-game.2d-geometry)
(cl-reexport:reexport-from :cl-web-2d-game.draw-model-system)
(cl-reexport:reexport-from :cl-web-2d-game.performance)
(cl-reexport:reexport-from :cl-web-2d-game.gui)
(cl-reexport:reexport-from :cl-web-2d-game.logger)
(cl-reexport:reexport-from :cl-web-2d-game.debug-drawer)
(cl-reexport:reexport-from :cl-web-2d-game.initializer)
