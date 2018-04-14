(in-package :cl-user)
(defpackage cl-web-2d-game/core/camera
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair)
  (:export :get-camera-offset-x
           :get-camera-offset-y
           :get-screen-width
           :get-screen-height
           :init-camera))
(in-package :cl-web-2d-game/core/camera)

(enable-ps-experiment-syntax)

(defvar.ps+ *camera-offset-x* 0)
(defvar.ps+ *camera-offset-y* 0)
(defvar.ps+ *screen-width* 0)
(defvar.ps+ *screen-height* 0)

(defun.ps+ get-camera-offset-x ()
  *camera-offset-x*)
(defun.ps+ get-camera-offset-y ()
  *camera-offset-y*)

(defun.ps+ get-screen-width ()
  *screen-width*)
(defun.ps+ get-screen-height ()
  *screen-height*)

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
    (setf *screen-width* width)
    (setf *screen-height* height)
    (camera.position.set 0 0 z)
    camera))
