(defpackage cl-web-2d-game/core/basic-components
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :make-vector-2d
           :vector-2d
           :vector-2d-p
           :vector-2d-x
           :vector-2d-y
           :x
           :y

           :make-point-2d
           :point-2d
           :point-2d-p
           :point-2d-x
           :point-2d-y
           :point-2d-angle
           :angle

           :make-speed-2d
           :speed-2d
           :speed-2d-p
           :speed-2d-x
           :speed-2d-y

           :make-rect-2d
           :rect-2d
           :rect-2d-p
           :rect-2d-x
           :rect-2d-y
           :rect-2d-width
           :rect-2d-height
           :width
           :height

           :make-rotate-2d
           :rotate-2d
           :rotate-2d-p
           :rotate-2d-speed
           :rotate-2d-angle
           :rotate-2d-radious

           :make-script-2d
           :script-2d
           :script-2d-func

           :params
           :params-table
           :get-entity-param
           :set-entity-param
           :aset-entity-param
           :init-entity-params

           :copy-vector-2d-to
           :clone-vector-2d
           :copy-point-2d-to
           :clone-point-2d))
(in-package :cl-web-2d-game/core/basic-components)

(enable-ps-experiment-syntax)

;; --- components --- ;;

(defstruct.ps+ (vector-2d (:include ecs-component)) (x 0) (y 0))
;; point-2d is mainly used as a local translation and rotation.
(defstruct.ps+ (point-2d (:include vector-2d)) (angle 0))
(defstruct.ps+ (speed-2d (:include vector-2d)))

(defstruct.ps+ (rect-2d (:include ecs-component)) (x 0) (y 0) (width 0) (height 0))

(defstruct.ps+ (rotate-2d (:include ecs-component)) (speed 0) (angle 0) (radious 0))

(defstruct.ps+ (params (:include ecs-component)) (table (make-hash-table)))

(defstruct.ps+ (script-2d (:include ecs-component)) (func (lambda (entity) entity)))

;; --- some functions --- ;;

;; - copy & clone

(defun.ps+ copy-vector-2d-to (dst-vector src-vector)
  (setf (vector-2d-x dst-vector) (vector-2d-x src-vector))
  (setf (vector-2d-y dst-vector) (vector-2d-y src-vector))
  dst-vector)

(defun.ps+ clone-vector-2d (vector)
  (with-slots (x y) vector
    (make-vector-2d :x x :y y)))

(defun.ps+ copy-point-2d-to (dst-point src-point)
  (setf (point-2d-x dst-point) (point-2d-x src-point))
  (setf (point-2d-y dst-point) (point-2d-y src-point))
  (setf (point-2d-angle dst-point) (point-2d-angle src-point))
  dst-point)

(defun.ps+ clone-point-2d (point)
  (with-slots (x y angle) point
    (make-point-2d :x x :y y :angle angle)))

;; - params

(defun.ps+ get-entity-param (entity key)
  (with-ecs-components (params) entity
    (gethash key (params-table params))))

(defun.ps+ set-entity-param (entity &rest key-value-pair)
  (let ((params (get-ecs-component 'params entity))
        (len (length key-value-pair)))
    (assert (= (mod len 2) 0))
    (unless params
      (setf params (make-params))
      (add-ecs-component params entity))
    ;; Note: Recursive style is better performance than "nth" style in CL.
    ;;       But it is opposite in JS...
    (loop :for i :from 0 :below (/ len 2) :do
       (let ((key (nth (* i 2) key-value-pair))
             (value (nth (1+ (* i 2)) key-value-pair)))
         (setf (gethash key (params-table params))
               value)))
    (nth (1- len) key-value-pair)))

(defsetf.ps+ get-entity-param
    (entity key) (new-value)
    `(set-entity-param ,entity ,key ,new-value))

(defmacro.ps+ aset-entity-param (entity key new-value)
  "Anapholic set-entity-param"
  `(let ((,(intern "IT" *package*) (get-entity-param ,entity ,key)))
     (set-entity-param ,entity ,key ,new-value)))

(defun.ps+ init-entity-params (&rest key-value-pairs)
  (unless (evenp (length key-value-pairs))
    (error "odd number of args to INIT-ENTITY-PARAMS"))
  (let* ((table (make-hash-table)))
    (labels ((rec (rest-pairs)
               (when (> (length rest-pairs) 0)
                 (setf (gethash (car rest-pairs) table)
                       (cadr rest-pairs))
                 (rec (cddr rest-pairs)))))
      (rec key-value-pairs))
    (make-params :table table)))
