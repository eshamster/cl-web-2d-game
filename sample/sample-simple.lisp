(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-simple
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-simple)

(use-this-package-as-sample :init-func init-func
                            :update-func update-func)

;; --- Parenscript program --- ;;

(defun.ps init-func (scene)
  (let* ((circle (make-ecs-entity))
         (r 20))
    (add-ecs-component-list
     circle
     (make-point-2d :x 50 :y 50)
     (make-model-2d :model (make-wired-regular-polygon :n 60 :color 0xff0000 :r r)
                    :offset (make-vector-2d :x (* -1 r) :y (* -1 r))
                    :depth 0))
    (add-ecs-entity circle))
  (init-default-systems :scene scene))

(defvar.ps *counter* 0)

(defun.ps update-func ()
  (incf *counter*)
  (add-to-monitoring-log *counter*)
  (do-ecs-entities entity
    (add-to-monitoring-log (ecs-entity-id entity)))
  (when (= (mod *counter* 60) 1)
    (add-to-event-log *counter*)))
