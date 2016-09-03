(in-package :cl-user)
(defpackage cl-web-2d-game.calc
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.basic-components)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair)
  (:export :incf-vector
           :decf-vector
           :incf-rotate-diff
           :decf-rotate-diff

           :calc-dist
           :calc-dist-to-line
           :calc-dist-to-line-seg))
(in-package :cl-web-2d-game.calc)

(enable-ps-experiment-syntax)

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


;; --- distance calculation functions --- ;;

(defun.ps+ calc-dist (pnt1 pnt2)
  (with-slots-pair (((x1 x) (y1 y)) pnt1
                    ((x2 x) (y2 y)) pnt2)
    (sqrt (+ (expt (- x2 x1) 2)
             (expt (- y2 y1) 2)))))

(defun.ps+ calc-dist-to-line (target-pnt line-pnt1 line-pnt2)
  (with-slots-pair (((x1 x) (y1 y)) line-pnt1
                    ((x2 x) (y2 y)) line-pnt2
                    ((xt x) (yt y)) target-pnt)
    (if (= x1 x2)
        (- xt x1)
        (let* ((slope (/ (- y2 y1) (- x2 x1)))
               (offset (- y1 (* slope x1))))
          (/ (- yt (* slope xt) offset)
             (sqrt (+ 1 (expt slope 2))))))))

(defvar.ps+ *origin-pnt* (make-vector-2d :x 0 :y 0))

(defun.ps+ calc-dist-to-line-seg (target-pnt line-pnt1 line-pnt2)
  ;; Preparation for calculation
  ;;   1. Transform coordinate to move line-pnt1 to origin
  ;;   2. Rotate coordinate around origin to move lint-pnt2 on x-axis
  ;; TODO: Reduce memory allocations
  (let ((moved-line-pnt2 (clone-vector line-pnt2))
        (moved-target-pnt (clone-vector target-pnt)))
    ;; 1
    (decf-vector moved-line-pnt2 line-pnt1)
    (decf-vector moved-target-pnt line-pnt1)
    ;; 2
    (let ((angle-pnt2 (vector-angle moved-line-pnt2)))
      (decf-rotate-diff moved-line-pnt2 moved-line-pnt2
                        0 angle-pnt2)
      (decf-rotate-diff moved-target-pnt moved-target-pnt
                        0 angle-pnt2))
    ;; rest calculations
    (with-slots-pair ((x y) moved-target-pnt
                      ((pnt2-x x)) moved-line-pnt2)
      (let ((left-x (min 0 pnt2-x))
            (right-x (max 0 pnt2-x)))
        (if (and (<= left-x x)
                 (<= x right-x))
            y
            (* (min (calc-dist moved-target-pnt moved-line-pnt2)
                    (calc-dist moved-target-pnt *origin-pnt*))
               (if (> y 0) 1 -1)))))))
