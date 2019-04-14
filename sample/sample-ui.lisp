(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-ui
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-ui)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defun.ps+ init-func (scene)
  (init-input)
  (init-default-systems :scene scene)
  (setf-collider-model-enable t)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d)
     (make-physic-rect :x 100 :y 100 :width 200 :height 100)
     (make-ui-component
      :on-mouse-enter (lambda ()
                        (add-to-event-log "mouse enter"))
      :on-mouse-leave (lambda ()
                        (add-to-event-log "mouse leave"))
      :on-mouse-hover (lambda ()
                        (add-to-monitoring-log "hover"))
      :on-mouse-not-hover (lambda ()
                            (add-to-monitoring-log "not hover"))
      :on-click-down (lambda ()
                       (add-to-event-log "mouse down"))
      :on-click-up (lambda ()
                     (add-to-event-log "mouse up"))))
    (add-ecs-entity entity)))

(defun.ps+ update-func ()
  )
