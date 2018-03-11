(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-animation
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-animation)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defun.ps add-explosion-model (&key x y depth texture-name)
  (let* ((rect (make-ecs-entity))
         (id-str ((ps:@ (+ "000" (ecs-entity-id rect)) slice) -4)))
    (add-to-event-log (+ id-str ": Created"))
    (add-ecs-component-list
     rect
     (make-point-2d :x x :y y
                    :angle (* 2 PI (random)))
     (make-model-2d :model (make-wired-rect :width 160 :height 160
                                            :color #x226622)
                    :depth (- depth 30)))
    (frame-promise-then
     (get-texture-promise texture-name)
     (lambda (texture)
       (let* ((model-2d (make-model-2d :model (make-texture-model :width 160 :height 160
                                                                  :texture texture)
                                       :depth depth))
              (anime-2d (init-animation-2d
                         :interval 4 :vert-count 5 :horiz-count 3
                         :model model-2d :texture texture
                         :animation-end-callback
                         (lambda (anime)
                           (case (floor (* 3 (random)))
                             (1 (reverse-animation anime)
                                (add-to-event-log (+ id-str ": Reversed")))
                             (2 (reset-animation anime :stop-p nil)
                                (add-to-event-log (+ id-str ": Repeated")))
                             (t (delete-ecs-entity rect)
                                (add-to-event-log (+ id-str ": Deleted"))))))))
         (add-ecs-component-list rect model-2d anime-2d))
       (start-animation anime-2d)
       (add-ecs-entity-to-buffer rect)))))

(defun.ps+ init-moving-animation (entity manager texture-name register-name)
  (let* ((width 120)
         (height width))
    (frame-promise-then
     (get-texture-promise texture-name)
     (lambda (texture)
       (let* ((model-2d (make-model-2d :model (make-texture-model :width width
                                                                  :height height
                                                                  :texture texture)
                                       :depth 50))
              (anime-2d (init-animation-2d
                         :interval 20 :vert-count 4
                         :model model-2d :texture texture)))
         (add-ecs-component-list
          entity
          (make-point-2d :x 200 :y 200)
          model-2d
          anime-2d)
         (register-animation manager register-name anime-2d))))))

(defun.ps+ add-moving-model ()
  (let* ((entity (make-ecs-entity))
         (manager (init-animation-manager entity)))
    (add-ecs-component-list
     entity
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (when (or (is-key-up-now :left)
                                       (is-key-up-now :right))
                               (reverse-current-animation manager))
                             (when (is-key-down-now :left)
                               (switch-current-animation manager "left"))
                             (when (is-key-down-now :right)
                               (switch-current-animation manager "right")))))
    (frame-promise-all
     (list (init-moving-animation entity manager "moving-left" "left")
           (init-moving-animation entity manager "moving-right" "right"))
     (lambda (values)
       (declare (ignore values))
       (add-ecs-entity entity)))))

(defun.ps load-textures ()
  (load-texture :path "/images/sample_explosion.png" :name "explosion"
                :alpha-path "/images/sample_explosion_alpha.png")
  (load-texture :path "/images/sample_moving.png" :name "moving-left"
                :alpha-path "/images/sample_moving_alpha.png"
                :height 0.5)
  (load-texture :path "/images/sample_moving.png" :name "moving-right"
                :alpha-path "/images/sample_moving_alpha.png"
                :y 0.5 :height 0.5))

(defun.ps init-func (scene)
  (set-console-log-level :debug)
  (load-textures)
  (add-moving-model)
  (init-default-systems :scene scene))

(defvar.ps *creation-interval* 60)
(defvar.ps *creation-interval-rest* 0)

(defun.ps count-entity ()
  (let ((count 0))
    (do-ecs-entities entity
      (incf count))
    count))

(defun.ps update-func ()
  (add-to-monitoring-log (+ "Num of explosion: " (1- (count-entity))))
  (decf *creation-interval-rest*)
  (when (<= *creation-interval-rest* 0)
    (setf *creation-interval-rest*
          (floor (* (random) *creation-interval*)))
    (add-explosion-model :texture-name "explosion"
                         :x (+ 80 (* (random 480)))
                         :y (+ 80 (* (random 320)))
                         :depth 10)))
