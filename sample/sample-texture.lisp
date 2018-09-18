(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-texture
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-texture)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defun.ps+ add-textured-model (&key x y depth texture-name
                                   rot-speed-ratio)
  (let ((rect (make-ecs-entity)))
    (add-ecs-component-list
     rect
     (make-point-2d :x x :y y)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (incf (point-2d-angle point-2d)
                                     (* rot-speed-ratio (/ PI 180))))))
     (make-model-2d :model (make-wired-rect :width 80 :height 80 :color #x00ff00)
                    :depth depth))
    (frame-promise-then
     (make-texture-model-promise
      :width 80 :height 80
      :texture-name texture-name)
     (lambda (model)
       (add-to-event-log (+ texture-name " has been loaded"))
       (add-ecs-component-list
        rect
        (make-model-2d :model model :depth depth))
       (add-ecs-entity rect)))))

(defun.ps+ add-textured-model-for-size-testing (&key x y width height size-type)
  (let ((rect (make-ecs-entity)))
    (add-ecs-component-list
     rect
     (make-point-2d :x x :y y))
    (frame-promise-then
     (make-texture-model-promise
      :width width :height height :size-type size-type
      :texture-name "multi-a")
     (lambda (model)
       (add-ecs-component-list
        rect
        (make-model-2d :model model :depth 100))
       (add-ecs-entity rect)))))

(defun.ps+ test-various-size ()
  ;; - relative - ;;
  ;; If size is not specified, the image is not scaled.
  (add-textured-model-for-size-testing
   :x 0 :y 450 :size-type :relative)
  ;; If either width or height is specified, the image is scaled keeping its aspect ratio.
  (add-textured-model-for-size-testing
   :x 120 :y 450 :width 0.5 :size-type :relative)
  (add-textured-model-for-size-testing
   :x 180 :y 450 :height 0.5 :size-type :relative)
  ;; (Specify both width and height.)
  (add-textured-model-for-size-testing
   :x 240 :y 450 :width 0.5 :height 0.8 :size-type :relative)

  ;; - absolute - ;;
  (add-textured-model-for-size-testing
   :x 360 :y 450 :size-type :absolute)
  (add-textured-model-for-size-testing
   :x 480 :y 450 :width 40 :size-type :absolute)
  (add-textured-model-for-size-testing
   :x 540 :y 450 :height 40 :size-type :absolute)
  (add-textured-model-for-size-testing
   :x 600 :y 450 :width 40 :height 80 :size-type :absolute))

(defun.ps+ load-textures ()
  (load-texture :path "/images/sample.png" :name "sample")
  (load-texture :path "/images/sample.png" :name "sample-alpha"
                :alpha-path "/images/sample_alpha.png")
  (load-texture :path "/images/multiple_image.png" :name "multi-a"
                :alpha-path "/images/multiple_image_alpha.png"
                :width 0.5)
  (load-texture :path "/images/multiple_image.png" :name "multi-b"
                :alpha-path "/images/multiple_image_alpha.png"
                :x 0.5 :width 0.5))

(defun.ps init-func (scene)
  (set-console-log-level :debug)
  (load-textures)
  ;; use "sample" or "sample-alpha"
  (add-textured-model :texture-name "sample"
                      :x 250 :y 250 :depth 10
                      :rot-speed-ratio -0.8)
  (add-textured-model :texture-name "sample-alpha"
                      :x 220 :y 220 :depth 30
                      :rot-speed-ratio 0.1)
  (add-textured-model :texture-name "sample-alpha"
                      :x 200 :y 200 :depth 20
                      :rot-speed-ratio 1)
  ;; use "multi-a" or "mutli-b"
  (add-textured-model :texture-name "multi-a"
                      :x 400 :y 100 :depth 10
                      :rot-speed-ratio 1.2)
  (add-textured-model :texture-name "multi-b"
                      :x 450 :y 150 :depth 20
                      :rot-speed-ratio -0.65)
  (test-various-size)
  (init-default-systems :scene scene))

(defun.ps update-func ())
