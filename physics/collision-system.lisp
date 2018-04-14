(in-package :cl-user)
(defpackage cl-web-2d-game/physics/collision-system
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/2d-geometry
        :cl-web-2d-game/graphics/draw-model-system
        :cl-web-2d-game/utils/calc
        :cl-web-2d-game/utils/debug/performance)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair)
  (:export :collision-system
           :make-collision-system
           :setf-collider-model-enable))
(in-package :cl-web-2d-game/physics/collision-system)

(enable-ps-experiment-syntax)

;; --- collider model --- ;;

(defvar.ps+ *collider-model-color* #x00ff00)
(defvar.ps+ *collider-model-depth* 10)
(defvar.ps+ *collider-model-enable* t)

(defun.ps+ generate-collider-model (physic-2d)
  (flet ((make-a-model (fn-make-geometry)
           (make-model-2d :model (funcall fn-make-geometry *collider-model-color*)
                          :depth *collider-model-depth*
                          :offset (clone-point-2d (physic-2d-offset physic-2d)))))
    (etypecase physic-2d
      (physic-circle (make-a-model
                      (lambda (color)
                        (make-wired-circle
                         :color color
                         :r (physic-circle-r physic-2d)))))
      (physic-polygon (make-a-model
                       (lambda (color)
                         (make-wired-polygon
                          :color color
                          :pnt-list (mapcar (lambda (pnt)
                                              (list (vector-2d-x pnt)
                                                    (vector-2d-y pnt)))
                                            (physic-polygon-pnt-list physic-2d)))))))))

(defun.ps+ add-collider-model (entity)
  ;; TODO: Avoid duplicated addition
  (register-next-frame-func
   (lambda ()
     (with-ecs-components (physic-2d) entity
       (add-ecs-component (generate-collider-model physic-2d) entity
                          physic-2d)))))

(defun.ps+ setf-collider-model-enable (value)
  (unless (eq value *collider-model-enable*)
    (setf *collider-model-enable* value)
    (do-ecs-entities entity
      (do-ecs-components-of-entity (comp entity
                                         :component-type 'physic-2d)
        (let ((model (find-a-component (lambda (target) (typep target 'model-2d))
                                       comp)))
          (if model
              (if value
                  (enable-model-2d entity :target-model-2d model)
                  (disable-model-2d entity :target-model-2d model))
              (when value
                (add-collider-model entity))))))
    t))

;; --- collision --- ;;

;; The with-ecs-components takes some time, so buffer
;; components before collision loop (double loop)
(defstruct.ps+ collision-entity-info
    entity global-point physic)

(defun.ps+ process-collision (entity1 ph1 pnt1 entity2 ph2 pnt2)
  (when (not (judge-collision-target-tags entity1 ph1 entity2 ph2))
    (return-from process-collision))
  (when (collide-physics-p ph1 pnt1 ph2 pnt2)
    (with-slots-pair (((event1 on-collision)) ph1
                      ((event2 on-collision)) ph2)
      (funcall event1 entity1 entity2)
      (funcall event2 entity2 entity1))))

(defstruct.ps+
    (collision-system
     (:include ecs-system
               (target-component-types '(point-2d physic-2d))
               (process-all
                (lambda (system)
                  (with-performance ("collision")
                    (with-slots ((entities target-entities)) system
                      (let ((info-list '()))
                        (dolist (entity entities)
                          (let ((physic (get-ecs-component 'physic-2d entity))
                                (global-point (calc-global-point entity)))
                            (update-bounding-box physic global-point)
                            (push (make-collision-entity-info
                                   :entity entity
                                   :global-point global-point
                                   :physic physic)
                                  info-list)))
                        (let ((length (length info-list)))
                          (loop for outer-idx from 0 below (1- length) do
                               (with-slots ((entity1 entity)
                                            (ph1 physic)
                                            (pnt1 global-point))
                                   (aref info-list outer-idx)
                                 (loop for inner-idx from (1+ outer-idx) below length do
                                      (with-slots ((entity2 entity)
                                                   (ph2 physic)
                                                   (pnt2 global-point))
                                          (aref info-list inner-idx)
                                        (process-collision entity1 ph1 pnt1
                                                           entity2 ph2 pnt2)))))))))))
               (add-entity-hook (lambda (entity)
                                  (when *collider-model-enable*
                                    (add-collider-model entity)))))))
