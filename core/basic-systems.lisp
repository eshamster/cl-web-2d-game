(defpackage cl-web-2d-game/core/basic-systems
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/animation
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/utils/debug/performance)
  (:import-from :cl-web-2d-game/utils/calc
                :incf-vector)
  (:export :script-system
           :make-script-system
           :animation-system
           :make-animation-system
           :make-simple-move-system))
(in-package :cl-web-2d-game/core/basic-systems)

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
    (simple-move-system
     (:include ecs-system
               (target-component-types '(point-2d speed-2d))
               (process (lambda (entity)
                          (with-ecs-components (point-2d speed-2d) entity
                            (incf-vector point-2d speed-2d)))))))
