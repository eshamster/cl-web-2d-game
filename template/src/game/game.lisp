(defpackage {{name}}/src/game/game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func)
  (:import-from :{{name}}/src/game/{{name}}-state
                :make-{{name}}-start-state))
(in-package :{{name}}/src/game/game)

(defun.ps+ init-func (scene)
  (init-game-state (make-{{name}}-start-state))
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (process-game-state))
