(in-package :cl-user)
(defpackage cl-web-2d-game.basic-systems
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game.animation
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.collision
        :cl-web-2d-game.performance)
  (:export :script-system
           :make-script-system
           :animation-system
           :make-animation-system
           :collision-system
           :make-collision-system))
(in-package :cl-web-2d-game.basic-systems)

(enable-ps-experiment-syntax)

(defstruct.ps+
    (script-system
     (:include ecs-system
               (target-component-types '(script-2d))
               (process (lambda (entity)
                          (with-ecs-components (script-2d) entity
                            (funcall (script-2d-func script-2d) entity)))))))

(defstruct.ps+
    (animation-system
     (:include ecs-system
               (target-component-types '(animation-2d))
               (process (lambda (entity)
                          (do-ecs-components-of-entity (anime entity
                                                              :component-type 'animation-2d)
                            (run-animation-process anime)))))))

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
                                          (process-collision entity1 ph1 entity2 ph2)))))))))))))))
