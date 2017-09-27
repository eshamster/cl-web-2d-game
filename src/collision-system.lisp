(in-package :cl-user)
(defpackage cl-web-2d-game.collision-system
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.collision
        :cl-web-2d-game.2d-geometry
        :cl-web-2d-game.draw-model-system
        :cl-web-2d-game.performance)
  (:export :collision-system
           :make-collision-system
           :setf-collider-model-enable))
(in-package :cl-web-2d-game.collision-system)

(enable-ps-experiment-syntax)

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
      (physic-triangle (make-a-model
                        (lambda (color)
                          (make-wired-polygon
                           :color color
                           :pnt-list (mapcar (lambda (pnt)
                                               (list (vector-2d-x pnt)
                                                     (vector-2d-y pnt)))
                                             (list (physic-triangle-pnt1 physic-2d)
                                                   (physic-triangle-pnt2 physic-2d)
                                                   (physic-triangle-pnt3 physic-2d)))))))
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

(defstruct.ps+
    (collision-system
     (:include ecs-system
               (target-component-types '(point-2d physic-2d))
               (process-all
                (lambda (system)
                  (with-performance ("collision")
                    (with-slots ((entities target-entities)) system
                      (let ((length (length entities)))
                        (loop for outer-idx from 0 below (1- length) do
                             (let ((entity1 (aref entities outer-idx)))
                               (with-ecs-components ((ph1 physic-2d)) entity1
                                 (loop for inner-idx from (1+ outer-idx) below length do
                                      (let ((entity2 (aref entities inner-idx)))
                                        (with-ecs-components ((ph2 physic-2d)) entity2
                                          (process-collision entity1 ph1 entity2 ph2))))))))))))
               (add-entity-hook (lambda (entity)
                                  (when *collider-model-enable*
                                    (add-collider-model entity)))))))
