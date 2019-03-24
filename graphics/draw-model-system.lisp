(in-package :cl-user)
(defpackage cl-web-2d-game/graphics/draw-model-system
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game/utils/calc)
  (:import-from :cl-web-2d-game/core/basic-components
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
           :model-2d-label
           :enable-model-2d
           :disable-model-2d
           :init-draw-model-system
           :update-model-2d
           :find-model-2d-by-label))
(in-package :cl-web-2d-game/graphics/draw-model-system)

(enable-ps-experiment-syntax)

;; --- component --- ;;

(defstruct.ps+ (model-2d (:include ecs-component))
    model
  (depth 0)
  (offset (make-point-2d))
  (state :invalid) ; :invalid, :enable, :disable
  label)

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
                            (do-ecs-components-of-entity
                                (modelc entity :component-type model-2d)
                              (let ((new-pos (calc-global-point
                                              entity (model-2d-offset modelc))))
                                (with-slots (model) modelc
                                  (model.position.set
                                   (point-2d-x new-pos)
                                   (point-2d-y new-pos)
                                   (model-2d-depth modelc))
                                  (setf model.rotation.z (point-2d-angle new-pos)))))))))))

(defvar.ps+ *scene-for-draw-system* nil)

(defun.ps enable-model-2d-if-state (entity target-state &key target-model-2d)
  ;; TODO: Check the entity has the target-model-2d if not nil
  (unless *scene-for-draw-system*
    (error "The scene for the draw system is not initialized"))
  (flet ((enable (target)
           (with-slots (model state) target
             (when (eq state target-state)
               (*scene-for-draw-system*.add model)
               (setf state :enable)))))
    (if target-model-2d
        (enable target-model-2d)
        (do-ecs-components-of-entity
            (modelc entity :component-type model-2d)
          (enable modelc)))))


(defun.ps enable-model-2d (entity &key target-model-2d)
  (enable-model-2d-if-state entity :disable
                            :target-model-2d target-model-2d))

(defun.ps enable-invalidated-model-2d (entity &key target-model-2d)
  (enable-model-2d-if-state entity :invalid
                            :target-model-2d target-model-2d))

(defun.ps disable-model-2d-if-required (target-model-2d)
  (with-slots (model state) target-model-2d
    (when (eq state :enable)
      (*scene-for-draw-system*.remove model)
      (setf state :disable))))

(defun.ps invalidate-model-2d (target-model-2d)
  (disable-model-2d-if-required target-model-2d)
  (with-slots (state) target-model-2d
    (setf state :invalid)))

(defun.ps invalidate-all-model-2d (entity)
  (do-ecs-components-of-entity
      (model entity :component-type model-2d)
    (invalidate-model-2d model)))

(defun.ps+ disable-model-2d (entity &key target-model-2d)
  ;; TODO: Check the entity has the target-model-2d if not nil
  (unless *scene-for-draw-system*
    (error "The scene for the draw system is not initialized"))
  (if target-model-2d
      (disable-model-2d-if-required target-model-2d)
      (do-ecs-components-of-entity
          (modelc entity :component-type 'model-2d)
        (disable-model-2d-if-required modelc))))

(defun.ps+ update-model-2d (entity old-model new-model)
  "Update old-model by new-model."
  (delete-ecs-component old-model entity)
  (add-ecs-component new-model entity))

(defun.ps+ find-model-2d-by-label (entity label)
  (do-ecs-components-of-entity
      (model entity :component-type 'model-2d)
    (when (eq (model-2d-label model) label)
      (return-from find-model-2d-by-label model)))
  (error "Can't find the label: ~A" label))

(defun.ps+ init-draw-model-system (scene)
  (setf *scene-for-draw-system* scene)
  (add-delete-component-hook
   (lambda (target) (invalidate-model-2d target)))
  (make-draw-model-system
   :add-entity-hook #'enable-invalidated-model-2d
   :delete-entity-hook #'invalidate-all-model-2d))
