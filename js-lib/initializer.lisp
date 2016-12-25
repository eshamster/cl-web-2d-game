(in-package :cl-user)
(defpackage cl-web-2d-game.initializer
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.collision
        :cl-web-2d-game.draw-model-system
        :cl-web-2d-game.utils)
  (:export :start-2d-game
           :init-default-system
           :get-rendered-dom))
(in-package :cl-web-2d-game.initializer)

(enable-ps-experiment-syntax)

(defvar.ps+ *rendered-dom* nil)

(defun.ps+ get-rendered-dom ()
  *rendered-dom*)

(defun.ps init-default-systems (&key scene
                                     (script-system t)
                                     (draw-system t)
                                     (collision-system t))
  (when script-system
    (register-ecs-system "script2d" (make-script-system)))
  (when collision-system
    (register-ecs-system "collision" (make-collision-system)))
  (when draw-system
    (register-ecs-system "draw2d" (init-draw-model-system scene))))

(defun.ps start-2d-game (&key screen-width screen-height
                              camera
                              rendered-dom
                              (init-function (lambda (scene) nil))
                              (update-function (lambda () nil)))
  "Entry point for starting game.
We assume that the camera is initalized using cl-web-2d-game[.camera]:init-camera."
  (let* ((scene (new (#j.THREE.Scene#)))
         (renderer (new #j.THREE.WebGLRenderer#)))
    (setf *rendered-dom* rendered-dom)
    (register-default-systems scene)
    (renderer.set-size screen-width screen-height)
    (chain rendered-dom
           (append-child renderer.dom-element))
    (let ((light (new (#j.THREE.DirectionalLight# 0xffffff))))
      (light.position.set 0 0.7 0.7)
      (scene.add light))
    (funcall init-function scene)
    (labels ((render-loop ()
               (request-animation-frame render-loop)
               (with-trace "render"
                 (renderer.render scene camera))
               (update-stats)
               (with-trace "update"
                 (funcall update-function))))
      (render-loop))))
