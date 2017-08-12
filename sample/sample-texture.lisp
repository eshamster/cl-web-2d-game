#!/bin/sh
#|-*- mode:lisp -*-|#
#| <Put a one-line description here>
exec ros -Q -- $0 "$@"
|#
;;; vim: set ft=lisp lisp:

(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(:ps-experiment
                              :cl-ps-ecs
                              :cl-web-2d-game
                              :ningle
                              :cl-markup
                              :clack)
                            :silent t))

(defpackage :ros.script.sample-texture
  (:use :cl
        :cl-markup
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :ps-experiment
                :defun.ps
                :defvar.ps))
(in-package :ros.script.sample-texture)

;; --- Definitions about directories --- ;;

(defvar *script-dir*
  (merge-pathnames "sample/"
                   (asdf:component-pathname
                    (asdf:find-system :cl-web-2d-game))))

(defvar *js-relative-dir* "js/")

(defvar *downloaded-js-dir*
  (merge-pathnames *js-relative-dir* *script-dir*))

;; --- Parenscript program --- ;;

(defvar *js-game-file*
  (merge-pathnames "sample.js" *downloaded-js-dir*))

(defun.ps add-textured-model (&key x y depth texture-name
                                   rot-speed-ratio)
  (let ((rect (make-ecs-entity)))
    (add-ecs-component-list
     rect
     (make-point-2d :x x :y y)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (incf (point-2d-angle point-2d)
                                     (* rot-speed-ratio (/ PI 180))))))
     (make-model-2d :model (make-wired-rect :width 80 :height 80 :color 0x00ff00)
                    :depth depth))
    (make-texture-model-async
     :width 80 :height 80
     :texture-name texture-name
     :callback (lambda (model)
                 (add-ecs-component-list
                  rect
                  (make-model-2d :model model :depth depth))
                 (add-ecs-entity rect)))))

(defun.ps load-textures ()
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
  (init-default-systems :scene scene))

(defun.ps update-func ()
  (do-ecs-entities entity
    (add-to-monitoring-log (ecs-entity-id entity))))

;; --- Make js main file --- ;;

(defun make-js-main-file ()
  (with-open-file (out *js-game-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ
     (pse:with-use-ps-pack (:this)
       (let ((width 640)
             (height 480))
         (start-2d-game :screen-width width
                        :screen-height height
                        :camera (init-camera 0 0 width height)
                        :rendered-dom (document.query-selector "#renderer")
                        :stats-dom (document.query-selector "#stats-output")
                        :monitoring-log-dom (document.query-selector "#monitor")
                        :event-log-dom (document.query-selector "#eventlog")
                        :init-function init-func
                        :update-function update-func)))
     out)))

;; --- Server --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defvar *server* nil)

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (make-js-main-file)
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "test")
                    (dolist (js-src (make-src-list-for-script-tag *js-relative-dir*))
                      (markup (:script :src js-src nil))))
                   (:body
                    (:div :id "stats-output")
                    (:div :id "renderer" nil)
                    (:div :id "monitor" "(for Monitoring Log)")
                    (:div (:pre :id "eventlog" "(for Event Log)"))
                    (:script :src "js/sample.js" nil)))))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun run (&key (port 5000))
  (ensure-js-files *downloaded-js-dir*)
  (stop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (:static :path (lambda (path)
                           (print path)
                           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)"
                                           path)
                               path
                               nil))
                   :root *script-dir*)
          *app*)
         :port port)))

;; --- Roswell script main --- ;;

(defun main (&rest argv)
  (declare (ignorable argv))
  (run :port 16896)
  (princ "--- Press enter key to stop ---")
  (peek-char)
  (stop))
