(in-package :cl-user)
(defpackage cl-web-2d-game-test.calc
  (:use :cl
        :prove
        :cl-ps-ecs)
  (:import-from :cl-web-2d-game-test.test-utils
                :use-packages-for-test)
  (:import-from :ps-experiment
                :defmacro.ps+)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both
                :prove-in-both
                :is-list.ps+)
  (:import-from :ps-experiment
                :defun.ps+
                :defvar.ps+)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game-test.calc)

;; --- prepare --- ;;

(use-packages-for-test :calc)

(defun.ps+ within-p (got expected tolerance)
  (< (- expected tolerance)
     got
     (+ expected tolerance)))

(defmacro.ps+ within (got expected tolerance)
  `(progn
     (is ,got ,expected :test (lambda (got expected) (within-p got expected ,tolerance)))))

(defvar.ps+ *angle-error* (/ PI 10000))
(defvar.ps+ *length-error* (/ 1 10000))

(defmacro.ps+ is-point (target x y angle)
  (with-gensyms (g-target)
    `(let ((,g-target ,target))
       (within (point-2d-x ,g-target) ,x *length-error*)
       (within (point-2d-y ,g-target) ,y *length-error*)
       (within (point-2d-angle ,g-target) ,angle *angle-error*))))

;; --- test --- ;;

(plan 6)

(subtest "Test vector calculations"
  (subtest "vector-abs"
    (with-prove-in-both ()
      (is (round (vector-abs
                  (make-vector-2d :x 3 :y 4)))
          5)
      (is (round (vector-abs
                  (make-vector-2d :x 4 :y -3)))
          5))) 
  (macrolet ((test-vector-angle (x y expected)
               ;; Because x and y is not passed to JavaScript environment,
               ;; use macro as an easy solution.
               `(prove-in-both (within (vector-angle (make-vector-2d :x ,x :y ,y))
                                       ,expected *angle-error*))))
    (subtest "vector-angle"
      (subtest "when x = 0"
        (test-vector-angle 0 100 (/ PI 2))
        (test-vector-angle 0 -100 (* -1 (/ PI 2))))
      (subtest "when x != 0"
        (test-vector-angle 100 0 0)
        (test-vector-angle -100 0 PI)
        (test-vector-angle  10  10 (* 1 (/ PI 4)))
        (test-vector-angle -10  10 (* 3 (/ PI 4)))
        (test-vector-angle -10 -10 (* -3 (/ PI 4)))
        (test-vector-angle  10 -10 (* -1 (/ PI 4)))))))

(subtest "Test vector modifications"
  (subtest "Test incf-vector and dicf-vector"
    (with-prove-in-both ()
      (let ((target (make-vector-2d :x 10 :y 10))
            (diff (make-vector-2d :x 5 :y -5)))
        (incf-vector target diff)
        (is (vector-2d-x target) 15)
        (is (vector-2d-y target) 5)
        (is (vector-2d-x diff) 5)
        (is (vector-2d-y diff) -5)))
    (with-prove-in-both ()
      (let ((target (make-vector-2d :x 10 :y 10))
            (diff (make-vector-2d :x 5 :y -5)))
        (decf-vector target diff)
        (is (vector-2d-x target) 5)
        (is (vector-2d-y target) 15)
        (is (vector-2d-x diff) 5)
        (is (vector-2d-y diff) -5))))
  (subtest "Test rotation functions"
    (subtest "setf-vector-angle"
      (with-prove-in-both ()
        (let* ((target (make-vector-2d :x 10 :y 10))
               (before-len (vector-abs target)))
          (flet ((prove-angle (expected-angle)
                   (within (- (vector-abs target) before-len)
                           0 *length-error*)
                   (within (vector-angle target)
                           expected-angle *angle-error*)))
            (setf-vector-angle target (* PI 1/3)) 
            (prove-angle (* PI 1/3))))))
    (subtest "incf-rotate-diff and decf-rotate-diff"
      ;; Now, test only case where the center of rotation is (0, 0)
      (with-prove-in-both ()
        (let* ((radious 10)
               (target (make-vector-2d :x radious :y 0)))
          (flet ((prove-angle (expected-angle)
                   (within (- (vector-abs target) radious)
                           0 0.0001)
                   (within (vector-angle target)
                           expected-angle *angle-error*)))
            (incf-rotate-diff target radious 0 (* PI 1/3))
            (prove-angle (* PI 1/3))
            (incf-rotate-diff target radious (* PI 1/3) (* PI 1/3))
            (prove-angle (* PI 2/3))
            (decf-rotate-diff target radious (* PI 2/3) (* PI 1/6))
            (prove-angle (* PI 1/2))))))
    (subtest "rotatef-point-by"
      (with-prove-in-both ()
        (let* ((radious 10)
               (target (make-point-2d :x radious :y 0))
               (speed (* PI 1/3))
               (rotator (make-rotate-2d :speed speed :radious radious)))
          (flet ((prove-angle (expected-angle)
                   (within (- (vector-abs target) radious)
                           0 *length-error*)
                   (within (vector-angle target)
                           expected-angle *angle-error*)
                   (within (point-2d-angle target)
                           expected-angle *angle-error*)
                   (within (rotate-2d-angle rotator)
                           expected-angle *angle-error*)))
            (rotatef-point-by target rotator)
            (prove-angle speed)
            (rotatef-point-by target rotator)
            (prove-angle (* speed 2))))))
    (subtest "adjustf-point-by-rotate"
      ;; Now, test only case where the center of rotation is (0, 0)
      (with-prove-in-both ()
        (let ((vector (make-vector-2d :x 0 :y 0)))
          (adjustf-point-by-rotate vector 5 (* PI 2/3))
          (within (vector-abs vector) 5 *length-error*)
          (within (vector-angle vector) (* PI 2/3) *angle-error*))))))

(subtest "Test functions about coordinate"
  (subtest "transformf-point"
    (with-prove-in-both ()
      ;; Note: (0, 0), (1, (sqrt 3)), (2, 0) are points of a regular triangle.
      (let ((base (make-point-2d :x 1 :y (sqrt 3) :angle (* PI -1/2)))
            (target (make-point-2d :x (sqrt 3) :y 1 :angle (* PI 1/4))))
        (transformf-point target base)
        ;; check the target is transformed
        (is-point target 2 0 (* PI -1/4))
        ;; check the base is not changed
        (is-point base 1 (sqrt 3) (* PI -1/2)))))
  (subtest "calc-global-point"
    (with-prove-in-both ()
      (let ((grand-parent (make-ecs-entity))
            (parent (make-ecs-entity))
            (stranger (make-ecs-entity))
            (child (make-ecs-entity)))
        (add-ecs-component (make-point-2d :x 0 :y 1 :angle (* PI -1/2))
                           parent)
        (add-ecs-component (make-point-2d :x 0 :y 1 :angle (* PI -1/2))
                           grand-parent)
        (add-ecs-component (make-point-2d :x 0 :y 1 :angle (* PI -1/2))
                           child)
        (setf (ecs-entity-parent child) stranger)
        (setf (ecs-entity-parent stranger) parent)
        (setf (ecs-entity-parent parent) grand-parent)
        (is-point (calc-global-point child) 1 0 (* PI -3/2))
        (let* ((offset (make-point-2d :x 0 :y 1 :angle (* PI -1/2)))
               (result (calc-global-point child offset)))
          (is-point result 0 0 (* PI -2))
          (is-point offset 0 1 (* PI -1/2)))))))

(subtest "Test point to point distance"
  (subtest "calc-dist"
    (with-prove-in-both ()
      (let ((pnt1 (make-vector-2d :x 2 :y -2))
            (pnt2 (make-vector-2d :x -1 :y 2))
            (expected 5)
            (tolerance *length-error*))
        (within (calc-dist pnt1 pnt2) expected tolerance)
        (within (calc-dist pnt2 pnt1) expected tolerance))))
  (subtest "calc-dist-p2"
    (with-prove-in-both ()
      (let ((pnt1 (make-vector-2d :x 2 :y -2))
            (pnt2 (make-vector-2d :x -1 :y 2))
            (expected 25)
            (tolerance 0.0001))
        (within (calc-dist-p2 pnt1 pnt2) expected tolerance)
        (within (calc-dist-p2 pnt2 pnt1) expected tolerance)))))

(subtest "Test point to line distance"
  (subtest "calc-dist-to-line"
    (with-prove-in-both ()
      (let ((line-pnt1 (make-vector-2d :x 0 :y 0))
            (line-pnt2 (make-vector-2d :x 1 :y 1)))
        (within (calc-dist-to-line (make-vector-2d :x 0 :y 1)
                                   line-pnt1 line-pnt2)
                (/ 1 (sqrt 2))
                *length-error*)
        (within (calc-dist-to-line (make-vector-2d :x 0 :y -1)
                                   line-pnt1 line-pnt2)
                (/ 1 (sqrt 2))
                *length-error*))
      ;; line-pnt1.x == line-pnt2.x
      (let ((line-pnt1 (make-vector-2d :x 1 :y 0))
            (line-pnt2 (make-vector-2d :x 1 :y 1)))
        (within (calc-dist-to-line (make-vector-2d :x 0 :y 1)
                                   line-pnt1 line-pnt2)
                1
                *length-error*)
        (within (calc-dist-to-line (make-vector-2d :x 0 :y -1)
                                   line-pnt1 line-pnt2)
                1
                *length-error*))))
  (subtest "calc-dist-to-line-seg"
    (with-prove-in-both ()
      (let ((line-pnt1 (make-vector-2d :x 0 :y 1))
            (line-pnt2 (make-vector-2d :x 1 :y 0)))
        (flet ((is-dist (x y expected)
                 (within (calc-dist-to-line-seg (make-vector-2d :x x :y y)
                                                line-pnt1 line-pnt2)
                         expected
                         *length-error*)))
          ;; The following points are on the line passing throw (2, 1) and (1, 2).
          (is-dist 3 0 2)
          (is-dist 2 1 (sqrt 2))
          (is-dist 1.5 1.5 (sqrt 2))
          (is-dist 1 2 (sqrt 2))
          (is-dist 0 3 2))))))

(subtest "Test misc."
  (subtest "adjust-to-target"
    (with-prove-in-both ()
      (is (adjust-to-target -1 8 2) 1)
      (is (adjust-to-target 7 8 2) 8)
      (is (adjust-to-target 8 -1 2) 6)
      (is (adjust-to-target 1 -1 2) -1)
      (is-error (adjust-to-target 1 -1 -1) 'simple-error)))
  (subtest "lerp-scalar"
    (with-prove-in-both ()
      (let ((tolerance 0.0001))
        (within (lerp-scalar 5 10 0)
                5 tolerance)
        (within (lerp-scalar 5 10 1)
                10 tolerance)
        (within (lerp-scalar 5 10 0.1)
                5.5 tolerance)
        (within (lerp-scalar 5 10 -0.1)
                4.5 tolerance)
        (within (lerp-scalar 5 10 1.1)
                10.5 tolerance)))))

(finalize)
