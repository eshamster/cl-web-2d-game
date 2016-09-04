(in-package :cl-user)
(defpackage :cl-web-2d-game
  (:use :cl-web-2d-game.basic-components
        :cl-web-2d-game.calc
        :cl-web-2d-game.collision)
  (:export
   ;; basic-components
   :make-vector-2d
   :vector-2d
   :vector-2d-p
   :vector-2d-x
   :vector-2d-y

   :point-2d
   :point-2d-p
   :point-2d-x
   :point-2d-y
   :point-2d-center
   :point-2d-angle

   :rotate-2d
   :rotate-2d-p
   :rotate-2d-speed
   :rotate-2d-angle
   :rotate-2d-rot-offset

   :model-2d
   :model-2d-p
   :model-2d-model
   :model-2d-depth

   ;; calc
   :incf-vector
   :decf-vector
   :incf-rotate-diff
   :decf-rotate-diff

   :calc-dist-to-line
   :calc-dist-to-line-seg

   ;; collision
   :process-collision
   :collision-system
   :make-collision-system

   :physic-2d
   :make-physic-2d

   :physic-circle
   :make-physic-circle

   :physic-triangle
   :make-physic-triangle))
