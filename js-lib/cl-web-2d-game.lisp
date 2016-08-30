(in-package :cl-user)
(defpackage :cl-web-2d-game
  (:use :cl-web-2d-game.basic-components)
  (:export
   ;; basic-components
   :make-vector-2d
   :vector-2d
   :vector-2d-x
   :vector-2d-y

   :point-2d
   :point-2d-x
   :point-2d-y
   :point-2d-center
   :point-2d-angle

   :model-2d
   :model-2d-model
   :model-2d-depth

   :incf-rotate-diff
   :decf-rotate-diff))
