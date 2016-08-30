(in-package :cl-user)
(defpackage cl-web-2d-game.basic-components
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :make-vector-2d
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
(in-package :cl-web-2d-game.basic-components)

(enable-ps-experiment-syntax)

;; --- components --- ;;

(defstruct.ps+ (vector-2d (:include ecs-component)) (x 0) (y 0))
(defstruct.ps+ (point-2d (:include vector-2d)) (center (make-vector-2d)) (angle 0))
(defstruct.ps+ (speed-2d (:include vector-2d)))

;; rot-offset (rotate offset) is defined as relative value from point-2d-center
(defstruct.ps+ (rotate-2d (:include ecs-component)) (speed 0) (angle 0) (rot-offset (make-vector-2d)))

(defstruct.ps+ (model-2d (:include ecs-component)) model (depth 0))

;; - vector functions - ;;

(defun.ps+ vector-abs (vector)
  (sqrt (+ (expt (vector-2d-x vector) 2)
           (expt (vector-2d-y vector) 2))))

(defun.ps+ vector-angle (vector)
  (with-slots (x y) vector
    (if (= x 0)
        0
        (+ (atan (/ y x))
           (if (< x 0) PI 0)))))

(defun.ps+ incf-vector (target-vec diff-vec)
  (incf (vector-2d-x target-vec) (vector-2d-x diff-vec))
  (incf (vector-2d-y target-vec) (vector-2d-y diff-vec))
  target-vec)

(defun.ps+ decf-vector (target-vec diff-vec)
  (decf (vector-2d-x target-vec) (vector-2d-x diff-vec))
  (decf (vector-2d-y target-vec) (vector-2d-y diff-vec))
  target-vec)

(defun.ps+ incf-rotate-diff (target-vector offset-vector now-angle diff-angle)
  (let* ((r (vector-abs offset-vector))
         (now-angle-with-offset (+ now-angle (vector-angle offset-vector)))
         (cos-now (cos now-angle-with-offset))
         (sin-now (sin now-angle-with-offset))
         (cos-diff (cos diff-angle))
         (sin-diff (sin diff-angle)))
    (with-slots (x y) target-vector
      (incf x (- (* r cos-now cos-diff)
                 (* r sin-now sin-diff)
                 (* r cos-now)))
      (incf y (- (+ (* r sin-now cos-diff)
                    (* r cos-now sin-diff))
                 (* r sin-now))))))

(defun.ps+ decf-rotate-diff (vector offset-vector now-angle diff-angle)
  (incf-rotate-diff vector offset-vector now-angle (* -1 diff-angle)))

(defun.ps+ calc-model-position (entity)
  (labels ((rec (result parent)
             (if parent
                 (let ((pos (get-ecs-component 'point-2d parent)))
                   (when pos
                     (incf-vector result pos)
                     (with-slots (center angle) pos
                       (if (eq entity parent)
                           (decf-vector result center)
                           (incf-rotate-diff result center 0 angle))))
                   (rec result (ecs-entity-parent parent)))
                 result)))
    (unless (get-ecs-component 'point-2d entity)
      (error "The entity ~A doesn't have point-2d" entity))
    (rec (make-vector-2d :x 0 :y 0) entity)))
