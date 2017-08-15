(in-package :cl-user)
(defpackage cl-web-2d-game.animation-manager
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.2d-geometry
        :cl-web-2d-game.animation
        :cl-web-2d-game.texture
        :cl-web-2d-game.draw-model-system
        :cl-web-2d-game.logger)
  (:export :animation-manager
           :init-animation-manager
           :switch-current-animation
           :reverse-current-animation
           :register-animation))
(in-package :cl-web-2d-game.animation-manager)

(enable-ps-experiment-syntax)

(defstruct.ps+ animation-manager
    entity
  (animation-table (make-hash-table))
  (current nil))

(defun.ps+ init-animation-manager (entity)
  (check-type entity ecs-entity)
  (make-animation-manager :entity entity))

(defun.ps+ switch-current-animation (manager name &key (forward-p t))
  (check-type manager animation-manager)
  (let ((next (find-animation-in-manager manager name)))
    (unless next
      (error "The animation \"~A\" is not registered" name))
    (with-slots (entity current) manager
      (when current
        (reset-animation current :stop-p t :forward-p t)
        (disable-animation entity current))
      (setf current next)
      (reset-animation current :stop-p nil :forward-p forward-p)
      (enable-animation entity current))))

(defun.ps+ reverse-current-animation (manager)
  (check-type manager animation-manager)
  (with-slots (current) manager
    (unless current
      (error "No animation is set as current."))
    (reverse-animation current)))

(defun.ps+ find-animation-in-manager (manager name)
  (gethash name
           (animation-manager-animation-table manager)))

(defun.ps+ register-animation (manager name anime-2d)
  (check-type manager animation-manager)
  (check-type anime-2d animation-2d)
  (with-slots (entity animation-table) manager
    (disable-animation entity anime-2d)
    (setf (gethash name animation-table) anime-2d)))
