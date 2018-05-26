(in-package :cl-user)
(defpackage cl-web-2d-game/graphics/text-area
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/font
        :cl-web-2d-game/graphics/2d-geometry
        :cl-web-2d-game/graphics/draw-model-system)
  (:export :make-text-area
           :add-text-to-area
           :clear-text-area
           :get-text-area-size
           
           :text-area-component
           :make-text-area-component))
(in-package :cl-web-2d-game/graphics/text-area)

(enable-ps-experiment-syntax)

(defstruct.ps+ (text-area-component (:include ecs-component))
  font-size
  text-align ; :left, :right, :center
  margin
  (text-model-list '())
  depth)

(defun.ps+ make-text-area (&key font-size (text-align :left) (margin 0) x y (angle 0))
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

(defun.ps+ get-text-area-size (area-entity)
  "Get area size as such plist as (:width xxx :height yyy)."
  (with-ecs-components ((area text-area-component)) area-entity
    (with-slots (font-size text-model-list margin) area
      (let ((max-width 0))
        (dolist (model text-model-list)
          (let ((width (get-mesh-width (model-2d-model model))))
            (when (> width max-width)
              (setf max-width width))))
        (list :width (+ max-width (* 2 margin))
              :height (+ margin
                         (* (+ font-size margin)
                            (length text-model-list))))))))

(defun.ps+ calc-aligned-offset-x (text-mesh align margin)
  (ecase align
    (:left (* -1 margin))
    (:right (* -1 (+ (get-mesh-width text-mesh) margin)))
    (:center (* -1 (/ (get-mesh-width text-mesh)
                      2)))))

(defun.ps+ add-text-to-area (area-entity &key text color)
  "Add a new text to the area. The text is attached to bottom of the previous one."
  (with-ecs-components ((area text-area-component)) area-entity
    (with-slots (font-size text-align text-model-list depth margin)
        area
      (frame-promise-then
       (make-text-model-promise text
                                :size font-size
                                :color color)
       (lambda (mesh)
         (let ((model (make-model-2d
                       :model mesh :depth depth
                       :offset (make-point-2d :x (calc-aligned-offset-x
                                                  mesh text-align margin)
                                              :y (* (+ margin
                                                       (* (+ font-size margin)
                                                          (1+ (length text-model-list))))
                                                    -1)))))
           (add-ecs-component-list area-entity model)
           (push model text-model-list))
         area-entity)))))

(defun.ps+ clear-text-area (area-entity)
  (with-ecs-components ((area text-area-component)) area-entity
    (with-slots (text-model-list) area
      (dolist (model text-model-list)
        (register-next-frame-func
         (lambda () (delete-ecs-component model area-entity))))
      (setf text-model-list '()))))
