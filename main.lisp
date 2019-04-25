(in-package :cl-user)

;; This package is depend on the following js libraries.
;; - three.js
;; - dat.gui.js
;; - threex.keyboardstate.js

;; Note: ":use" not only re-exporting is also requred. Because
;; ps-experiment:with-use-ps-pack solves dependencies using package-use-list.

(uiop/package:define-package :cl-web-2d-game/main
  (:nicknames :cl-web-2d-game)
  (:use-reexport :cl-web-2d-game/core/basic-components
                 :cl-web-2d-game/core/basic-systems
                 :cl-web-2d-game/core/initializer
                 :cl-web-2d-game/core/game-state
                 ;; Note: The camera is maybe not "core".
                 :cl-web-2d-game/core/camera

                 :cl-web-2d-game/graphics/texture
                 :cl-web-2d-game/graphics/animation
                 :cl-web-2d-game/graphics/animation-manager
                 :cl-web-2d-game/graphics/2d-geometry
                 :cl-web-2d-game/graphics/draw-model-system
                 :cl-web-2d-game/graphics/font
                 :cl-web-2d-game/graphics/text-area

                 :cl-web-2d-game/inputs/input
                 :cl-web-2d-game/inputs/gui
                 :cl-web-2d-game/inputs/ui

                 :cl-web-2d-game/physics/collision
                 :cl-web-2d-game/physics/collision-system

                 :cl-web-2d-game/utils/utils
                 :cl-web-2d-game/utils/calc 
                 :cl-web-2d-game/utils/debug/performance
                 :cl-web-2d-game/utils/debug/logger
                 :cl-web-2d-game/utils/debug/debug-drawer
                 :cl-web-2d-game/utils/basic-generator
                 :cl-web-2d-game/utils/stage-generator
                 :cl-web-2d-game/utils/storage))
(in-package :cl-web-2d-game/main)
