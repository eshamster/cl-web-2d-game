(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-basic-models
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-basic-models)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defun.ps+ add-a-model (x y depth model)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y)
     (make-model-2d :model model
                    :depth depth))
    (add-ecs-entity entity)))

(defun.ps+ init-models ()
    ;; line
  (add-a-model 50 50 20
               (make-line :pos-a (list 10 10)
                          :pos-b (list 20 50)
                          :color #x00ff00))
  (add-a-model 100 50 20
               (make-lines :pnt-list '((10 10) (20 50) (12 65) (-5 25))
                           :color #xff0000))
  ;; rect
  (add-a-model 150 50 20
               (make-wired-rect :width 20 :height 50
                                :color #x00ff00))
  (add-a-model 200 50 20
               (make-solid-rect :width 20 :height 50
                                :color #x00ff00))
  ;; regular polygon
  (add-a-model 250 50 20
               (make-wired-regular-polygon :r 20 :n 5 :color #xff00ff))
  (add-a-model 300 50 20
               (make-wired-regular-polygon :r 20 :n 5 :start-angle 30))
  (add-a-model 350 50 20
               (make-solid-regular-polygon :r 20 :n 5 :color #xff00ff))
  (add-a-model 400 50 20
               (make-solid-regular-polygon :r 20 :n 5 :start-angle 30))
  ;; circle
  (add-a-model 450 50 20
               (make-solid-circle :r 20 :color #x00ffff))
  (add-a-model 500 50 20
               (make-wired-circle :r 20 :color #x00ffff))
  ;; polygon
  (let ((pnts '((10 10) (20 50) (12 65) (-5 25))))
    (add-a-model 50 150 20
                 (make-wired-polygon :pnt-list pnts
                                     :color #x00ffff))
    (add-a-model 100 150 20
                 (make-solid-polygon :pnt-list pnts
                                     :color #x00ffff)))
  ;; for enabling and disabling model
  (let ((entity (make-ecs-entity))
        (model1 (make-model-2d :model (make-wired-regular-polygon
                                       :r 30 :n 4
                                       :color #xff0000)))
        (model2 (make-model-2d :model (make-wired-regular-polygon
                                       :r 30 :n 60
                                       :color #xff0000)))
        (interval 30)
        (rest-interval 20)
        (current-stage 0))
    (add-ecs-component-list
     entity
     (make-point-2d :x 300 :y 200)
     model1
     model2
     (make-script-2d
      :func (lambda (entity)
              (decf rest-interval)
              (add-to-monitoring-log (+ "Cycle: " current-stage))
              (when (<= rest-interval 0)
                (setf rest-interval interval)
                (incf current-stage)
                (case current-stage
                  (1 (disable-model-2d entity :target-model-2d model1))
                  (2 (disable-model-2d entity))
                  (3 (enable-model-2d entity))
                  (4 (disable-model-2d entity))
                  (5 (enable-model-2d entity :target-model-2d model2))
                  (6 (enable-model-2d entity))
                  (t (setf current-stage 0)))))))
    (add-ecs-entity entity)))

(defvar.ps+ *current-color* #xffffff)

(defun.ps+ init-gui-panel ()
  (init-gui)
  ;; for test change-model-color
  (let ((entity (add-a-model 200 200 20
                             (make-solid-rect :width 50 :height 50
                                              :color *current-color*))))
    (add-panel-number "change color" 1 :min 0 :max 1 :step 0.01
                      :on-change (lambda (value)
                                   (with-ecs-components (model-2d) entity
                                     (let ((color (+ #xff0000
                                                     (* #x100 #xff value)
                                                     (* #xff value))))
                                       (change-model-color model-2d color)
                                       (setf *current-color* color)))))))

(defun.ps+ init-func (scene)
  (set-console-log-level :debug)
  (init-default-systems :scene scene)
  (init-models)
  (init-gui-panel))

(defun.ps update-func ()
  (add-to-monitoring-log (+ "Color: 0x" (ps:chain (-math.floor *current-color*)
                                                  (to-string 16)
                                                  (to-upper-case)))))
