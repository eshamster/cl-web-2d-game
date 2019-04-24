(defpackage cl-web-2d-game/utils/basic-generator
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :cl-web-2d-game/core/basic-components
                :make-point-2d
                :make-speed-2d)
  (:import-from :cl-web-2d-game/utils/stage-generator
                :def-stage-element-interpreter.ps+))
(in-package :cl-web-2d-game/utils/basic-generator)

(def-stage-element-interpreter.ps+ :point (x y angle)
  (make-point-2d :x x :y y :angle angle))

(def-stage-element-interpreter.ps+ :speed (x y)
  (make-speed-2d :x x :y y))
