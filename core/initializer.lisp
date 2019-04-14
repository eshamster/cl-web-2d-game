(in-package :cl-user)
(defpackage cl-web-2d-game/core/initializer
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/core/basic-systems
        :cl-web-2d-game/graphics/draw-model-system
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/physics/collision-system
        :cl-web-2d-game/inputs/input
        :cl-web-2d-game/utils/utils
        :cl-web-2d-game/utils/debug/logger
        :cl-web-2d-game/utils/debug/performance)
  (:export :start-2d-game
           :init-default-systems)
  (:import-from :cl-web-2d-game/utils/dom-manager
                :get-rendered-dom)
  (:import-from :cl-web-2d-game/inputs/ui
                :init-ui-system))
(in-package :cl-web-2d-game/core/initializer)

(enable-ps-experiment-syntax)

(defvar.ps *stats* nil)

(defun.ps init-stats (stats-dom)
  (setf *stats* (new (-stats)))
  (let ((stats *stats*))
    (stats.set-mode 0)
    (with-slots (position left top) stats.dom-element.style
      (setf position "absolute")
      (setf left "0px")
      (setf top "0px"))
    (chain (document.get-element-by-id "stats-output")
           (append-child stats.dom-element))
    stats))

(defun.ps update-stats ()
  (when *stats*
    (*stats*.update)))

(defun.ps+ init-default-systems (&key scene
                                      (script-system t)
                                      (draw-system t)
                                      (animation-system t)
                                      (collision-system t)
                                      (simple-move-system t)
                                      (ui-system t))
  (when script-system
    (register-ecs-system "script2d" (make-script-system)))
  (when collision-system
    (register-ecs-system "collision" (make-collision-system)))
  (when animation-system
    (register-ecs-system "animation" (make-animation-system)))
  (when draw-system
    (register-ecs-system "draw2d" (init-draw-model-system scene)))
  (when simple-move-system
    (register-ecs-system "simple-move" (make-simple-move-system)))
  (when ui-system
    (register-ecs-system "ui" (init-ui-system))))

(defvar.ps+ *resize-to-screen-p* nil)

;; TODO: Fix the following issue
;; This is a temporal solution to avoid unintentional scroll bar
;; when the height size eqauls to 100% of the screen height.
;; In such case, 7px area is appeared both in top and bottom.
;; But the cause is not revealed.
(defvar.ps+ *window-height-adjust* 14)

(defun.ps initialize-screen-size (rendered-dom renderer screen-width screen-height resize-to-screen-p)
  (setf *resize-to-screen-p* resize-to-screen-p)
  (labels ((calc-scale ()
             (min (/ window.inner-width screen-width)
                  (/ (- window.inner-height *window-height-adjust*) screen-height)))
           (set-position-by-size (width height)
             (setf rendered-dom.style.position "absolute"
                   rendered-dom.style.left (+ (/ (- window.inner-width width) 2) "px")
                   rendered-dom.style.top (+ (/ (- window.inner-height height) 2) "px")))
           (set-size (width height)
             (renderer.set-size width height)
             (set-position-by-size width height))
           (resize ()
             (let ((scale (if *resize-to-screen-p* (calc-scale) 1)))
               (set-size (* screen-width scale)
                         (* screen-height scale)))))
    (resize)
    (let ((resize-timer nil))
      (window.add-event-listener
       "resize" (lambda (e)
                  (declare (ignore e))
                  (when resize-timer
                    (clear-timeout resize-timer))
                  (setf resize-timer
                        (set-timeout (lambda () (resize))
                                     100)))))))

;; Note: There is two parameteres related to width (height) of screen.
;; One means width (height) of HTML canvas. It is specified by "screen-width"
;; ("screen-height") in "start-2d-game".
;; Another means width (height) of space in camera. It is specified by "width"
;; ("height") in "cl-web-2d-game[.camera]:init-camera".
;; The "cl-web-2d-game[.camera]:get-screen-width(height)" returns the latter.
(defun.ps start-2d-game (&key screen-width screen-height
                              camera
                              rendered-dom
                              stats-dom
                              monitoring-log-dom
                              event-log-dom
                              (resize-to-screen-p nil)
                              (init-function (lambda (scene) nil))
                              (update-function (lambda () nil)))
  "Entry point for starting game.
We assume that the camera is initalized using cl-web-2d-game[.camera]:init-camera."
  (let* ((scene (new (#j.THREE.Scene#)))
         (renderer (new #j.THREE.WebGLRenderer#)))
    (setf (get-rendered-dom) rendered-dom)
    (when stats-dom
      (init-stats stats-dom))
    (init-monitoring-log monitoring-log-dom)
    (init-event-log-area event-log-dom)
    (initialize-screen-size rendered-dom renderer
                            screen-width screen-height
                            resize-to-screen-p)
    (chain rendered-dom
           (append-child renderer.dom-element))
    (let ((light (new (#j.THREE.DirectionalLight# 0xffffff))))
      (light.position.set 0 0.7 0.7)
      (scene.add light))
    (funcall init-function scene)
    (labels ((render-loop ()
               (request-animation-frame render-loop)
               (with-performance ("loop-top")
                 (with-performance ("render")
                   (with-trace "render"
                     (renderer.render scene camera)))
                 (update-stats)
                 (with-performance ("update")
                   (with-trace "update"
                     (clear-monitoring-log)
                     (process-input)
                     (ecs-main)
                     (funcall update-function))))
               (add-to-monitoring-log (dump-performance-counter))))
      (render-loop))))
