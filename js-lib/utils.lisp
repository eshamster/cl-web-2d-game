(in-package :cl-user)
(defpackage cl-web-2d-game.utils
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :start-2d-game
           :with-trace
           :convert-to-layered-hash
           :get-layered-hash))
(in-package :cl-web-2d-game.utils)

(enable-ps-experiment-syntax)

;; --- performance tracer --- ;;

;; Note: this is depend on Web Tracing Framework (wtf-trace.js)

(defmacro.ps with-trace (title &body body)
  `(let ((scope (#j.WTF.trace.enterScope# ,title)))
     ,@body
     (#j.WTF.trace.leaveScope# scope ,title)))

(defmacro with-trace (title &body body)
  "(dummy)"
  (declare (ignore title))
  `(progn ,@body))

;; --- game starter --- ;;

(defun.ps start-2d-game (&key screen-width screen-height
                              camera
                              rendered-dom
                              (init-function (lambda (scene) nil))
                              (update-function (lambda () nil)))
  "Entry point for starting game.
We assume that the camera is initalized using cl-web-2d-game[.camera]:init-camera."
  (let* ((scene (new (#j.THREE.Scene#)))
         (renderer (new #j.THREE.WebGLRenderer#)))
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

;; --- constant value manager --- ;;

(defun map-pair (function list)
    "Ex. (map-pair (lambda (a b) (+ a b)) '(1 2 3 4)) => '(3 7)"
    (labels ((rec (rest result)
               (if rest
                   (rec (cddr rest)
                        (cons (funcall function (car rest) (cadr rest)) result))
                   result)))
      (nreverse (rec list nil))))

#|
Example:
(defvar *hash*
  (convert-to-layered-hash
   (:position (:x 12 :y (+ 10 20))
    :size (:width (* 2 3) :height 100)
    :some-list (list 1 2 3))))

(get-layered-hash *hash* :position :x)  => 12
(get-layered-hash *hash* :position :y)  => 30
(get-layered-hash *hash* :size :width)  => 6
(get-layered-hash *hash* :size :height) => 100
(get-layered-hash *hash* :some-list) => (1 2 3)
|#
(defmacro.ps+ convert-to-layered-hash (list)
  (labels ((is-pair (element)
             (and (listp element)
                  (string= (package-name (symbol-package (car element)))
                           "KEYWORD")))
           (convert-value (value)
             (if (listp value)
                 `(lambda () ,value)
                 value))
           (make-hash-insertion (rest)
             `(let ((result (make-hash-table)))
                ,@(map-pair (lambda (key value)
                              `(setf (gethash ,key result)
                                     ,(if (is-pair value)
                                          (make-hash-insertion value)
                                          (convert-value value))))
                            rest)
                result)))
    (make-hash-insertion list)))

(defmacro.ps+ get-layered-hash (hash &rest keys)
  (labels ((rec (rest-keys result)
             (if rest-keys
                 (rec (cdr rest-keys)
                      `(gethash ,(car rest-keys) ,result))
                 result)))
    `(let ((value ,(rec keys hash)))
       (if (functionp value)
           (funcall value)
           value))))
