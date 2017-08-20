(in-package :cl-user)
(defpackage cl-web-2d-game.draw-model-system
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game.calc)
  (:import-from :cl-web-2d-game.basic-components
                :point-2d
                :point-2d-x
                :point-2d-y
                :point-2d-angle
                :make-point-2d)
  (:export :make-model-2d
           :model-2d
           :model-2d-p
           :model-2d-model
           :model-2d-depth
           :model-2d-offset
           :model-2d-geometry
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

(defmacro.ps model-2d-geometry (model)
  `(progn (check-type ,model model-2d)
          (@ ,model model geometry)))

(defmacro model-2d-geometry (model)
  (declare (ignore model))
  `(error "model-2d-geometry for CL is not implemented"))

;; --- system --- ;;

(defstruct.ps
    (draw-model-system
     (:include ecs-system
               (target-component-types '(point-2d model-2d))
               (process (lambda (entity)
                          (with-ecs-components (point-2d) entity
                            (do-ecs-components-of-entity (modelc entity
                                                                 :component-type model-2d)

                              (let ((new-pos (calc-global-point entity
                                                                (model-2d-offset modelc))))
                                (with-slots (model) modelc
                                  (model.position.set
                                   (point-2d-x new-pos)
                                   (point-2d-y new-pos)
                                   (model-2d-depth modelc))
                                  (setf model.rotation.z (point-2d-angle new-pos)))))))))))

(defvar.ps+ *scene-for-draw-system* nil)

;; TODO: Don't enabel if the entity is not registered in the draw-model-system.
(defun.ps enable-model-2d (entity &key target-model-2d)
  ;; TODO: Check the entity has the target-model-2d if not nil
  (unless *scene-for-draw-system*
    (error "The scene for the draw system is not initialized"))
  (flet ((enable (target)
           (with-slots (model enable) target
             (unless enable
               (*scene-for-draw-system*.add model)
               (setf enable t)))))
    (if target-model-2d
        (enable target-model-2d)
        (do-ecs-components-of-entity (modelc entity
                                             :component-type model-2d)
          (enable modelc)))))

(defun.ps disable-model-2d (entity &key target-model-2d)
  ;; TODO: Check the entity has the target-model-2d if not nil
  (unless *scene-for-draw-system*
    (error "The scene for the draw system is not initialized"))
  (flet ((disable (target)
           (with-slots (model enable) target
             (*scene-for-draw-system*.remove model)
             (setf enable nil))))
    (if target-model-2d
        (disable target-model-2d)
        (do-ecs-components-of-entity (modelc entity
                                             :component-type 'model-2d)
          (disable modelc)))))

(defun.ps+ init-draw-model-system (scene)
  (setf *scene-for-draw-system* scene)
  (make-draw-model-system
   :add-entity-hook #'enable-model-2d
   :delete-entity-hook #'disable-model-2d))
