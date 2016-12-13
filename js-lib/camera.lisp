(in-package :cl-user)
(defpackage cl-web-2d-game.camera
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair)
  (:export :get-camera-offset-x
           :get-camera-offset-y
           :init-camera))
(in-package :cl-web-2d-game.camera)

(enable-ps-experiment-syntax)

(defvar.ps+ *camera-offset-x* 0)
(defvar.ps+ *camera-offset-y* 0)

(defun.ps+ get-camera-offset-x ()
  *camera-offset-x*)
(defun.ps+ get-camera-offset-y ()
  *camera-offset-y*)

(defun.ps init-camera (offset-x offset-y width height)
  (let* ((x offset-x)
         (y offset-y)
         (z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       (* x -1) (- width x)
                       (- height y) (* y -1)
                       0 (* z 2)))))
    (setf *camera-offset-x* offset-x)
    (setf *camera-offset-y* offset-y)
    (camera.position.set 0 0 z)
    camera))
