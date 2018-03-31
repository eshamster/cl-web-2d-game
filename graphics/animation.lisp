(defpackage cl-web-2d-game/graphics/animation
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/2d-geometry
        :cl-web-2d-game/graphics/texture
        :cl-web-2d-game/graphics/draw-model-system
        :cl-web-2d-game/utils/debug/logger)
  (:export :animation-2d
           :init-animation-2d
           :start-animation
           :start-reversed-animation
           :reverse-animation
           :reset-animation
           :stop-animation
           :enable-animation
           :disable-animation
           :run-animation-process))
(in-package :cl-web-2d-game/graphics/animation)

(enable-ps-experiment-syntax)

;; TODO: Enable to repeat animation

(defstruct.ps+ (animation-2d (:include ecs-component))
    ;; input parameter
    interval (horiz-count 1) (vert-count 1) model texture
    animation-end-callback
    ;; state parameter
    (goes-to-forward t)
    (runs-animation nil)
    (interval-counter 0)
    (image-counter 0))

(defun.ps+ init-animation-2d (&key interval (horiz-count 1) (vert-count 1) model texture
                                   (animation-end-callback (lambda (anime) (declare (ignore anime)))))
  (check-type model model-2d)
  (check-type texture texture-2d)
  (let ((anime (make-animation-2d :interval interval
                                  :horiz-count horiz-count
                                  :vert-count vert-count
                                  :model model
                                  :texture texture
                                  :animation-end-callback animation-end-callback)))
    (switch-animation-image anime 0)
    anime))

(defun.ps+ enable-animation (entity anime-2d)
  "Enable drawing the model"
  (check-type entity ecs-entity)
  (check-type anime-2d animation-2d)
  (enable-model-2d entity
                   :target-model-2d (animation-2d-model anime-2d)))

(defun.ps+ disable-animation (entity anime-2d)
  "Stop the animation and disable drawing the model"
  (check-type entity ecs-entity)
  (check-type anime-2d animation-2d)
  (stop-animation anime-2d)
  (disable-model-2d entity
                    :target-model-2d (animation-2d-model anime-2d)))

(defun.ps+ start-animation (anime)
  (with-slots (goes-to-forward interval-counter interval runs-animation) anime
    (unless goes-to-forward
      (setf interval-counter
            (- interval interval-counter 1)))
    (setf runs-animation t)
    (setf goes-to-forward t)))

(defun.ps+ start-reversed-animation (anime)
  (with-slots (goes-to-forward interval-counter interval runs-animation) anime
    (when goes-to-forward
      (setf interval-counter
            (- interval interval-counter 1)))
    (setf runs-animation t)
    (setf goes-to-forward nil)))

(defun.ps+ reverse-animation (anime)
  (if (animation-2d-goes-to-forward anime)
      (start-reversed-animation anime)
      (start-animation anime)))

(defun.ps+ reset-animation (anime &key (stop-p t) (forward-p :asis))
  (with-slots (goes-to-forward interval-counter runs-animation
                               horiz-count vert-count) anime
    (when (eq forward-p :asis)
      (setf forward-p goes-to-forward))
    (setf interval-counter 0)
    (switch-animation-image anime
                            (if forward-p
                                0
                                (1- (* horiz-count vert-count))))
    (setf runs-animation (not stop-p))
    (setf goes-to-forward forward-p)))

(defun.ps+ stop-animation (anime)
  (setf (animation-2d-runs-animation anime) nil))

(defun.ps+ switch-animation-image (anime next-counter)
  (with-slots (model texture image-counter horiz-count vert-count) anime
    (let ((max-count (* horiz-count vert-count)))
      (when (or (< next-counter 0)
                (>= next-counter max-count))
        (error "The target animation counter is invalid (Max: ~D, Got: ~D)"
               max-count next-counter))
      (setf image-counter next-counter)
      (let ((x-count (mod next-counter vert-count))
            (y-count (- horiz-count (floor next-counter vert-count) 1))
            (width (/ 1.0 vert-count))
            (height (/ 1.0 horiz-count)))
        (change-geometry-uvs texture (model-2d-geometry model)
                             (* width x-count) (* height y-count)
                             width height)))))

(defun.ps+ run-animation-process (anime)
  (with-slots (runs-animation goes-to-forward interval interval-counter
                              image-counter horiz-count vert-count
                              animation-end-callback) anime
    (when runs-animation
      (if (< (1+ interval-counter) interval)
          (incf interval-counter)
          (if (or (and goes-to-forward (< (1+ image-counter)
                                          (* horiz-count vert-count)))
                  (and (not goes-to-forward) (> image-counter 0)))
              (progn (setf interval-counter 0)
                     (switch-animation-image anime
                                             (if goes-to-forward
                                                 (1+ image-counter)
                                                 (1- image-counter))))
              (progn (setf runs-animation nil)
                     (funcall animation-end-callback anime)))))))
