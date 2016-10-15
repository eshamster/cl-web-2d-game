(in-package :cl-user)
(defpackage cl-web-2d-game.draw-model-system
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:import-from :cl-web-2d-game.basic-components
                :point-2d
                :point-2d-x
                :point-2d-y
                :point-2d-angle
                :make-point-2d)
  (:export :model-2d
           :model-2d-p
           :model-2d-model
           :model-2d-depth
           :model-2d-offset
           :enable-model-2d
           :disable-model-2d
           :init-draw-model-system))
(in-package :cl-web-2d-game.draw-model-system)

(enable-ps-experiment-syntax)

;; --- component --- ;;

(defstruct.ps+ (model-2d (:include ecs-component))
    model
  (depth 0)
  (offset (make-point-2d))
  (enable nil))

;; --- system --- ;;

(defstruct.ps
    (draw-model-system
     (:include ecs-system
               (target-component-types '(point-2d model-2d))
               (process (lambda (entity)
                          (with-ecs-components (model-2d point-2d) entity
                            (let ((new-pos (calc-global-point entity
                                                              (model-2d-offset model-2d))))
                              (with-slots (model) model-2d
                                (model.position.set
                                 (point-2d-x new-pos)
                                 (point-2d-y new-pos)
                                 (model-2d-depth model-2d)) 
                                (setf model.rotation.z (point-2d-angle new-pos))))))))))

(defvar.ps *scene-for-draw-system* nil)

;; TODO: Don't enabel if the entity is not registered in the draw-model-system.
(defun.ps enable-model-2d (entity)
  (unless *scene-for-draw-system*
    (error "The scene for the draw system is not initialized"))
  (with-ecs-components (model-2d) entity
    (with-slots (model enable) model-2d
      (unless enable
        (*scene-for-draw-system*.add model)
        (setf enable t)))))

(defun.ps disable-model-2d (entity)
  (unless *scene-for-draw-system*
    (error "The scene for the draw system is not initialized"))
  (with-ecs-components (model-2d) entity
    (with-slots (model enable) model-2d
      (*scene-for-draw-system*.remove model)
      (setf enable nil))))

(defun.ps init-draw-model-system (scene)
  (setf *scene-for-draw-system* scene)
  (make-draw-model-system
   :add-entity-hook #'enable-model-2d
   :delete-entity-hook #'disable-model-2d))
