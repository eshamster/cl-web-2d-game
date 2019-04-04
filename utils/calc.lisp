(in-package :cl-user)
(defpackage cl-web-2d-game/utils/calc
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair)
  (:import-from :cl-web-2d-game/utils/utils
                :def-obsoleted-alias.ps+)
  (:export :vector-2d-abs
           :vector-2d-angle
           :setf-vector-2d-abs
           :setf-vector-2d-angle
           :add-vector-2d
           :sub-vector-2d
           :incf-vector-2d
           :decf-vector-2d
           :calc-inner-product
           :calc-outer-product-z
           :incf-rotate-diff
           :decf-rotate-diff
           :rotatef-point-by
           :movef-vector-to-circle

           :multf-vec-scalar
           :*-vec-scalar
           :divf-vec-scalar
           :/-vec-scalar

           :truncatef-vector-2d
           :truncate-vector-2d

           :transformf-point
           :transformf-point-inverse
           :calc-global-point
           :calc-local-point
           :with-global-point

           :diff-angle

           :calc-dist
           :calc-dist-p2
           :calc-dist-to-line
           :calc-dist-to-line-seg

           :adjust-to-target
           :rotate-to-target-angle
           :lerp-scalar
           :lerp-vector-2d

           ;; obsoleted
           :vector-abs
           :vector-angle
           :setf-vector-abs
           :setf-vector-angle
           :incf-vector
           :decf-vector
           :add-vector-2d))
(in-package :cl-web-2d-game/utils/calc)

(enable-ps-experiment-syntax)

(defun.ps+ vector-2d-abs (vector)
  (with-slots (x y) vector
    (sqrt (+ (expt x 2)
             (expt y 2)))))

(def-obsoleted-alias.ps+ vector-abs vector-2d-abs)

(defun.ps+ vector-2d-angle (vector)
  "Return the angle of the vector. The range is (-PI, PI].
The angle of the vector (1, 0) is 0 and the rotation is counterclockwize."
  (with-slots (x y) vector
    (if (= x 0)
        (* (/ PI 2)
           (if (< y 0) -1 1))
        (+ (atan (/ y x))
           (if (< x 0)
               (* PI (if (< y 0) -1 1))
               0)))))

(def-obsoleted-alias.ps+ vector-angle vector-2d-angle)

(defun.ps+ setf-vector-2d-abs-angle (vector abs angle)
  (setf (vector-2d-x vector) (* abs (cos angle))
        (vector-2d-y vector) (* abs (sin angle)))
  vector)

(def-obsoleted-alias.ps+ setf-vector-abs-angle setf-vector-2d-abs-angle)

(defun.ps+ setf-vector-2d-abs (vector abs)
  "Set the absolute length of the vector keeping its angle."
  (setf-vector-2d-abs-angle vector abs (vector-2d-angle vector)))

(def-obsoleted-alias.ps+ setf-vector-abs setf-vector-2d-abs)

(defun.ps+ setf-vector-2d-angle (vector angle)
  "Set the angle of the vector keeping its length."
  (setf-vector-2d-abs-angle vector (vector-2d-abs vector) angle))

(def-obsoleted-alias.ps+ setf-vector-angle setf-vector-2d-angle)

(defun.ps+ incf-vector-2d (target-vec diff-vec)
  "Destructively increase vector"
  (incf (vector-2d-x target-vec) (vector-2d-x diff-vec))
  (incf (vector-2d-y target-vec) (vector-2d-y diff-vec))
  target-vec)

(def-obsoleted-alias.ps+ incf-vector incf-vector-2d)

;; Note: Prefer "+-vecotr-2d" but it is converted to "vector2d" by ps:ps...
(defun.ps+ add-vector-2d (&rest vectors)
  (if (= (length vectors) 0)
      (make-vector-2d)
      (let ((result (clone-vector-2d (car vectors))))
        (dolist (vec (cdr vectors))
          (incf-vector-2d result vec))
        result)))

(defun.ps+ decf-vector-2d (target-vec diff-vec)
  "Destructively decrease vector"
  (decf (vector-2d-x target-vec) (vector-2d-x diff-vec))
  (decf (vector-2d-y target-vec) (vector-2d-y diff-vec))
  target-vec)

(def-obsoleted-alias.ps+ decf-vector decf-vector-2d)

;; Note: Prefer "--vecotr-2d" but it is converted to "vector2d" by ps:ps...
(defun.ps+ sub-vector-2d (&rest vectors)
  (if (= (length vectors) 0)
      (make-vector-2d)
      (let ((result (clone-vector-2d (car vectors))))
        (dolist (vec (cdr vectors))
          (decf-vector-2d result vec))
        result)))

(defun.ps+ calc-inner-product (vec1 vec2)
  (+ (* (vector-2d-x vec1) (vector-2d-x vec2))
     (* (vector-2d-y vec1) (vector-2d-y vec2))))

(defun.ps+ calc-outer-product-z (vec1 vec2)
  "Calculate z component of outer-product.
It is useful to judge which side a point is from a vector."
  (- (* (vector-2d-x vec1) (vector-2d-y vec2))
     (* (vector-2d-y vec1) (vector-2d-x vec2))))

(defun.ps+ incf-rotate-diff (target-vector radious now-angle diff-angle)
  "Rotate the target-vector.
The vector is rotated from 'now-angle' to '(+ now-angle diff-angle)'
on the circle whose radious is represented by the 'radious'."
  (let* ((r radious)
         (cos-now (cos now-angle))
         (sin-now (sin now-angle))
         (cos-diff (cos diff-angle))
         (sin-diff (sin diff-angle)))
    (with-slots (x y) target-vector
      (incf x (- (* r cos-now cos-diff)
                 (* r sin-now sin-diff)
                 (* r cos-now)))
      (incf y (- (+ (* r sin-now cos-diff)
                    (* r cos-now sin-diff))
                 (* r sin-now))))))

(defun.ps+ decf-rotate-diff (vector radious now-angle diff-angle)
  (incf-rotate-diff vector radious now-angle (* -1 diff-angle)))

(defun.ps+ rotatef-point-by (point-2d rotate-2d)
  "Rotate point-2d using rotate-2d structure."
  (let ((speed (rotate-2d-speed rotate-2d)))
    (incf-rotate-diff point-2d
                      (rotate-2d-radious rotate-2d)
                      (rotate-2d-angle rotate-2d)
                      speed)
    (incf (point-2d-angle point-2d) speed)
    (incf (rotate-2d-angle rotate-2d) speed)))

(defun.ps+ movef-vector-to-circle (vector radious angle)
  "Move the vector to a point of cirle parameterized by
radious and angle assuming that it is at the center of the
rotation."
  (incf (point-2d-x vector) radious)
  (incf-rotate-diff vector radious 0 angle)
  (when (typep vector 'point-2d)
    (incf (point-2d-angle vector) angle)))

(defun.ps+ transformf-point (target base)
  "Transform the 'target' point to the coordinate represented by the 'base'"
  (incf (point-2d-angle target) (point-2d-angle base))
  (with-slots-pair (((place-x x) (place-y y)) target
                    (x y angle) base)
    (let ((cos-value (cos angle))
          (sin-value (sin angle))
          (before-x place-x)
          (before-y place-y))
      (setf place-x (+ x
                       (- (* before-x cos-value) (* before-y sin-value))))
      (setf place-y (+ y
                       (+ (* before-x sin-value) (* before-y cos-value))))))
  target)

(defun.ps+ transformf-point-inverse (target base)
  "Reverse the transform of the \"transformf-point\"."
  (decf (point-2d-angle target) (point-2d-angle base))
  (with-slots-pair (((place-x x) (place-y y)) target
                    (x y angle) base)
    (let ((cos-value (cos (* -1 angle)))
          (sin-value (sin (* -1 angle)))
          (before-x place-x)
          (before-y place-y))
      (setf place-x (- (* (- before-x x) cos-value) (* (- before-y y) sin-value)))
      (setf place-y (+ (* (- before-x x) sin-value) (* (- before-y y) cos-value)))))
  target)

(defun.ps+ transformf-point-rec (base-pnt parent)
  (if parent
      (let ((pos (get-ecs-component 'point-2d parent)))
        (when pos
          (transformf-point base-pnt pos))
        (transformf-point-rec base-pnt (ecs-entity-parent parent)))
      base-pnt))

(defun.ps+ calc-global-point (entity &optional offset)
  "Return global position and roration of the entity (type: point-2d).
Note that an entity has only local position and rotation on coordinate of its parent.
The 'offset' is useful if you want to calculate global position and rotation on
coordinate of the 'entity'"
  (unless (get-ecs-component 'point-2d entity)
    (error "The entity ~A doesn't have point-2d" entity))
  (transformf-point-rec
   (if offset
       (clone-point-2d offset)
       (make-point-2d :x 0 :y 0 :angle 0))
   entity))

(defun.ps+ calc-local-point (entity global-pnt)
  "Calculate local position and rotation of the entity from a global point."
  (unless (get-ecs-component 'point-2d entity)
    (error "The entity ~A doesn't have point-2d" entity))
  (let ((base-pnt
         (transformf-point-rec
          (make-point-2d :x 0 :y 0 :angle 0) (ecs-entity-parent entity)))
        (result (clone-point-2d global-pnt)))
    (transformf-point-inverse result base-pnt)
    result))

(defmacro.ps+ with-global-point ((var entity) &body body)
  "The entiti'es global point is bound to the var.
After the body is evaluated, the entiti'es local point is updated by the var."
  (let ((g-entity (gensym)))
    `(let* ((,g-entity ,entity)
            (,var (calc-global-point ,g-entity)))
       (unwind-protect
            (progn ,@body)
         (copy-point-2d-to (get-ecs-component 'point-2d ,g-entity)
                           (calc-local-point ,g-entity ,var))))))

;; --- calculation between vector and scalar --- ;;

(defun.ps+ calcf-vec-scalar (vector scalar func)
  (with-slots (x y) vector
    (setf x (funcall func x scalar)
          y (funcall func y scalar)))
  vector)

(defun.ps+ multf-vec-scalar (vector scalar)
  "Destructively multiply each component of vector by scalar"
  (calcf-vec-scalar vector scalar (lambda (a b) (* a b))))

(defun.ps+ *-vec-scalar (vector scalar)
  "Multiply each component of vector by scalar"
  (multf-vec-scalar (clone-vector-2d vector) scalar))

(defun.ps+ divf-vec-scalar (vector scalar)
  "Destructively divide each component of vector by scalar"
  (calcf-vec-scalar vector scalar (lambda (a b) (/ a b))))

(defun.ps+ /-vec-scalar (vector scalar)
  "Divide each component of vector by scalar"
  (divf-vec-scalar (clone-vector-2d vector) scalar))

;; --- truncation --- ;;

(defun.ps+ truncatef-vector-2d (vec max-length)
  "Destructively truncate vector length to max-length or less"
  (when (> (vector-2d-abs vec) max-length)
    (setf-vector-2d-abs vec max-length))
  vec)

(defun.ps+ truncate-vector-2d (vec max-length)
  "Truncate vector length to max-length or less"
  (truncatef-vector-2d (clone-vector-2d vec) max-length))

;; --- angle calculation functions --- ;;

(defun.ps+ diff-angle (angle1 angle2)
  "-PI < result <= PI"
  (let ((raw-diff (- angle1 angle2)))
    (loop while (<= raw-diff (* -1 PI))
       do (incf raw-diff (* 2 PI)))
    (loop while (> raw-diff PI)
       do (decf raw-diff (* 2 PI)))
    raw-diff))

;; --- distance calculation functions --- ;;

(defun.ps+ calc-dist (pnt1 pnt2)
  "Calculate distance from pnt1 to pnt2."
  (sqrt (calc-dist-p2 pnt1 pnt2)))

(defun.ps+ calc-dist-p2 (pnt1 pnt2)
  "Calculate square of distance from pnt1 to pnt2.
Because this doesn't use 'sqrt' that is heavy calculation, you should use this
instead of 'calc-dist' if possible."
  (+ (expt (- (vector-2d-x pnt2) (vector-2d-x pnt1)) 2)
     (expt (- (vector-2d-y pnt2) (vector-2d-y pnt1)) 2)))

(defun.ps+ calc-dist-to-line (target-pnt line-pnt1 line-pnt2)
  "Calculate distance from a target-point to a line passin through line-pnt1 and line-pnt2."
  (with-slots-pair (((x1 x) (y1 y)) line-pnt1
                    ((x2 x) (y2 y)) line-pnt2
                    ((xt x) (yt y)) target-pnt)
    (abs (if (= x1 x2)
             (- xt x1)
             (let* ((slope (/ (- y2 y1) (- x2 x1)))
                    (offset (- y1 (* slope x1))))
               (/ (- yt (* slope xt) offset)
                  (sqrt (+ 1 (expt slope 2)))))))))

(defvar.ps+ *origin-pnt* (make-vector-2d :x 0 :y 0))

(defun.ps+ calc-dist-to-line-seg (target-pnt line-pnt1 line-pnt2)
  "Calculate distance from a target-point to a line segment whose endponits are
line-pnt1 and line-pnt2."
  ;; Preparation for calculation
  ;;   1. Transform coordinate to move line-pnt1 to origin
  ;;   2. Rotate coordinate around origin to move lint-pnt2 on x-axis
  ;; TODO: Reduce memory allocations
  (let ((moved-line-pnt2 (clone-vector-2d line-pnt2))
        (moved-target-pnt (clone-vector-2d target-pnt)))
    ;; 1
    (decf-vector-2d moved-line-pnt2 line-pnt1)
    (decf-vector-2d moved-target-pnt line-pnt1)
    ;; 2
    (let ((angle-pnt2 (vector-2d-angle moved-line-pnt2)))
      (labels ((rotate-pnt (now-pnt)
                 (decf-rotate-diff now-pnt (vector-2d-abs now-pnt)
                                   (vector-2d-angle now-pnt) angle-pnt2)))
        (rotate-pnt moved-line-pnt2)
        (rotate-pnt moved-target-pnt)))
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

;; --- others --- ;;

(defun.ps+ adjust-to-target (now-value target-value max-diff)
  "Move now-value closer to taret-value. But the max difference from now-value is limited by max-diff."
  (unless (> max-diff 0)
    (error "The 'max-diff' parameter should be a positive number."))
  (let ((diff (- target-value now-value)))
    (if (< (abs diff) max-diff)
        target-value
        (if (> diff 0)
            (+ now-value max-diff)
            (- now-value max-diff)))))

(defun.ps+ rotate-to-target-angle (now-angle target-angle max-diff)
  "Rotate now-angle closer to taret-angle. But the max difference from now-angle is limited by max-diff."
  (unless (> max-diff 0)
    (error "The 'max-diff' parameter should be a positive number."))
  (let ((diff (diff-angle now-angle target-angle)))
    (cond ((<= (abs diff) max-diff) target-angle)
          ((> diff 0) (decf now-angle max-diff))
          (t (incf now-angle max-diff)))))

(defun.ps+ lerp-scalar (min-value max-value alpha)
  "Linear interpolation function for scalars. alpha = 0 -> min-value, alpha = 1 -> max-value"
  (+ min-value
     (* alpha (- max-value min-value))))

(defun.ps+ lerp-vector-2d (min-vector max-vector alpha &optional (place (make-vector-2d)))
  "Linear interpolation function for vector-2d. alpha = 0 -> min-value, alpha = 1 -> max-value"
  (setf (vector-2d-x place) (lerp-scalar (vector-2d-x min-vector)
                                         (vector-2d-x max-vector)
                                         alpha)
        (vector-2d-y place) (lerp-scalar (vector-2d-y min-vector)
                                         (vector-2d-y max-vector)
                                         alpha))
  place)
