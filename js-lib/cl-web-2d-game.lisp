(in-package :cl-user)

;; This package is depend on the following js libraries.
;; - three.js
;; - dat.gui.js
;; - threex.keyboardstate.js

(defpackage :cl-web-2d-game
  (:use :cl-web-2d-game.basic-components
        :cl-web-2d-game.utils
        :cl-web-2d-game.calc
        :cl-web-2d-game.camera
        :cl-web-2d-game.collision
        :cl-web-2d-game.input
        :cl-web-2d-game.2d-geometry
        :cl-web-2d-game.draw-model-system
        :cl-web-2d-game.gui
        :cl-web-2d-game.initializer)
  (:export
   ;; basic-components
   :vector-abs
   :vector-angle
   :setf-vector-angle
   :make-vector-2d
   :vector-2d
   :vector-2d-p
   :vector-2d-x
   :vector-2d-y

   :make-point-2d
   :point-2d
   :point-2d-p
   :point-2d-x
   :point-2d-y
   :point-2d-center
   :point-2d-angle

   :rotate-2d
   :rotate-2d-p
   :rotate-2d-speed
   :rotate-2d-angle

   :script-2d
   :script-2d-func
   :make-script-system

   :params
   :params-table
   :get-entity-param
   :set-entity-param
   :init-entity-params

   :copy-vector-2d-to
   :clone-vector
   :copy-point-2d
   :clone-point-2d

   ;; utils
   :with-trace
   :convert-to-layered-hash
   :get-layered-hash

   ;; calc
   :incf-vector
   :decf-vector
   :incf-rotate-diff
   :decf-rotate-diff
   :rotatef-point-by
   :adjustf-point-by-rotate

   :calc-global-point

   :calc-dist
   :calc-dist-p2
   :calc-dist-to-line
   :calc-dist-to-line-seg

   :adjust-to-target
   :lerp-scalar

   ;; camera
   :get-camera-offset-x
   :get-camera-offset-y
   :init-camera

   ;; collision
   :collide-entities-p
   :collision-system
   :make-collision-system

   :physic-2d
   :make-physic-2d

   :physic-circle
   :make-physic-circle

   :physic-triangle
   :make-physic-triangle

   ;; input
   :add-mouse-down-callback
   :add-mouse-up-callback
   :add-mouse-move-callback
   :add-touch-start-callback
   :add-touch-end-callback
   :add-touch-move-callback

   :mouse-event-x
   :mouse-event-y

   :touch-event-touches
   :touch-event-element-x
   :touch-event-element-y

   :initialize-input

   ;; 2d-geometry
   :make-line
   :make-lines
   :make-solid-rect
   :make-wired-rect
   :make-solid-regular-polygon
   :make-wired-regular-polygon
   :make-wired-polygon
   :make-solid-polygon
   :change-model-color

   ;; draw-model-system
   :model-2d
   :model-2d-p
   :model-2d-model
   :model-2d-depth
   :model-2d-offset
   :enable-model-2d
   :disable-model-2d
   :init-draw-model-system

   ;; gui
   :init-gui
   :add-panel-bool
   :add-panel-number
   :add-panel-button

   ;; initializer
   :start-2d-game
   :init-default-system
   :get-rendered-dom))
