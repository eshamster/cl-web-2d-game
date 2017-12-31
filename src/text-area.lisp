(in-package :cl-user)
(defpackage cl-web-2d-game.text-area
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.font
        :cl-web-2d-game.2d-geometry
        :cl-web-2d-game.draw-model-system)
  (:export :make-text-area
           :add-text-to-area))
(in-package :cl-web-2d-game.text-area)

(enable-ps-experiment-syntax)

(defstruct.ps+ (text-area-component (:include ecs-component))
  font-size
  text-align ; :left, :right, :center
  margin
  (text-mesh-list '())
  depth)

(defun.ps+ make-text-area (&key font-size text-align margin x y (angle 0))
  "Make an empty text area as an ECS entity.
Note: \"y\" is top of the text. \"x\" depends on \"text-align\""
  (let ((text-align-list '(:left :right :center)))
    (when (not (find text-align text-align-list))
      (error "The allowed kinds of text-align: ~A" text-align-list)))
  (let ((area (make-ecs-entity)))
    (add-ecs-component-list
     area
     (make-point-2d :x x :y y :angle angle)
     (make-text-area-component
      :font-size font-size
      :text-align text-align
      :margin margin))
    area))

(defun.ps get-mesh-width (mesh)
  mesh.geometry.bounding-box.max.x)

(defun.ps+ calc-aligned-offset-x (text-mesh align margin)
  (ecase align
    (:left (* -1 margin))
    (:right (* -1 (+ (get-mesh-width text-mesh) margin)))
    (:center (* -1 (/ (get-mesh-width text-mesh)
                      2)))))

(defun.ps+ add-text-to-area (area-entity &key text color)
  "Add a new text to the area. The text is attached to bottom of the previous one."
  (with-ecs-components ((area text-area-component)) area-entity
    (with-slots (font-size text-align text-mesh-list depth margin)
        area
      (frame-promise-then
       (make-text-model-promise text
                                :size font-size
                                :color color)
       (lambda (mesh)
         (add-ecs-component-list
          area-entity
          (make-model-2d :model mesh :depth depth
                         :offset (make-point-2d :x (calc-aligned-offset-x mesh
                                                                          text-align
                                                                          margin)
                                                :y (* (+ margin
                                                         (* (+ font-size margin)
                                                            (1+ (length text-mesh-list))))
                                                      -1))))
         (push mesh text-mesh-list)
         area-entity)))))
