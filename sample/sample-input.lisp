(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-input
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-input)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defun.ps+ init-func (scene)
  (init-input)
  (init-default-systems :scene scene)
  (let ((background (make-ecs-entity)))
    (add-ecs-component-list
     background
     (make-point-2d)
     (make-model-2d :model (make-solid-rect :width 800 :height 600
                                            :color #xcccccc)
                    :depth 0))
    (add-ecs-entity background)))

(defun.ps+ display-key-state ()
  (dolist (key (list :a :b :c :left :right :up :down))
    (add-to-monitoring-log
     (+ key "(" (get-physical-key-name key) "): "
        (cond ((is-key-down-now key) "down-now")
              ((is-key-up-now key) "up-now")
              ((is-key-down key) "down")
              ((is-key-up key) "up")
              (t (error "Error: Unrecognized key state")))))))

(defun.ps+ display-mouse-state ()
  (add-to-monitoring-log
   (+ "Position: x=" (floor (get-mouse-x)) ", y=" (floor (get-mouse-y))))
  (add-to-monitoring-log
   (+ "Left: " (get-left-mouse-state)))
  (add-to-monitoring-log
   (+ "Right: " (get-right-mouse-state)))
  (add-to-monitoring-log
   (+ "Wheel:" (get-mouse-wheel-delta-y))))

(defun.ps+ display-touch-state ()
  (add-to-monitoring-log (+ "total: "
                            ", x: " (floor (get-total-touch-x))
                            ", y: " (floor (get-total-touch-y))
                            ", state: " (get-total-touch-state)))
  (do-touch-state (id)
    (add-to-monitoring-log (+ "id: " id
                              ", x: " (floor (get-touch-x id))
                              ", y: " (floor (get-touch-y id))
                              ", state: " (get-touch-state id)))))

(defun.ps+ update-func ()
  (add-to-monitoring-log "")
  (add-to-monitoring-log "--- Keyboard state ---")
  (display-key-state)
  (add-to-monitoring-log "--- Mouse state ---")
  (display-mouse-state)
  (add-to-monitoring-log "--- Touch state ---")
  (display-touch-state)
  ;; TODO: Add sample codes for callbacks
  )
