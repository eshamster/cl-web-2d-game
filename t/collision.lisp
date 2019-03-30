(defpackage cl-web-2d-game/t/collision
  (:use :cl
        :rove
        :cl-ps-ecs
        :ps-experiment/t/test-utils
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/utils/calc
        :cl-web-2d-game/t/test-utils)
  (:import-from :ps-experiment
                :defun.ps+))
(in-package :cl-web-2d-game/t/collision)

;; --- test --- ;;

(defun.ps+ same-bool-p (a b)
  "This is required because 'false' and 'null' is not same in JavaScript."
  (when (or (and a b)
            (and (not a) (not b)))
    t))

(deftest.ps+ circle-to-circle
  (labels ((cols-2-circles (r1 x1 y1 r2 x2 y2)
             (collide-physics-p
              (make-physic-circle :r r1) (make-point-2d :x x1 :y y1)
              (make-physic-circle :r r2) (make-point-2d :x x2 :y y2))))
    (testing "A circle includes another circle"
      (ok (cols-2-circles 2 0 0 1 0 0)))
    (testing "Circles cross"
      (ok (cols-2-circles 2 1 0 1 0 0)))
    (testing "doesn't collide"
      (ng (cols-2-circles 1 -10 1 3 1 1)))))

(deftest.ps+ circle-to-polygon
  ;; Use a regular hexagon as a polygon
  (let ((polygon (make-physic-polygon
                  :pnt-list (list (make-point-2d :x  2 :y 0)
                                  (make-point-2d :x  1 :y (sqrt 3))
                                  (make-point-2d :x -1 :y (sqrt 3))
                                  (make-point-2d :x -2 :y 0)
                                  (make-point-2d :x -1 :y (* -1 (sqrt 3)))
                                  (make-point-2d :x  1 :y (* -1 (sqrt 3)))))))
    (labels ((test-cp (rc xc yc pnt-polygon)
               (collide-physics-p
                (make-physic-circle :r rc) (make-point-2d :x xc :y yc)
                polygon pnt-polygon)))
      (testing "A circle includes a polygon"
        (ok (test-cp 3 0 0 (make-point-2d))))
      (testing "A circle is included in a polygon"
        (ok (test-cp 1 0 0 (make-point-2d))))
      (testing "A circle crosses to a polygon"
        (ok (test-cp 1 2 0 (make-point-2d))))
      (testing "Doesn't collide in default, but collides when a polygon rotates"
        (labels ((test-rotate (angle)
                   (test-cp 1.05 0 3 (make-point-2d :angle angle))))
          (ng (test-rotate 0))
          (ok (test-rotate (/ PI 6))))))))

(deftest.ps+ polygon-to-polygon
  ;; Use a regular hexagon as a polygon
  (flet ((make-test-polygon (r)
           (let ((pnt-list (list (make-point-2d :x  2 :y 0)
                                 (make-point-2d :x  1 :y (sqrt 3))
                                 (make-point-2d :x -1 :y (sqrt 3))
                                 (make-point-2d :x -2 :y 0)
                                 (make-point-2d :x -1 :y (* -1 (sqrt 3)))
                                 (make-point-2d :x  1 :y (* -1 (sqrt 3))))))
             (dolist (pnt pnt-list)
               (setf-vector-2d-abs pnt r))
             (make-physic-polygon :pnt-list pnt-list))))
    (testing "A polygon includes another"
      (ok (collide-physics-p
           (make-test-polygon 2) (make-point-2d)
           (make-test-polygon 1) (make-point-2d))))
    (testing "Two polygons cross"
      (ok (collide-physics-p
           (make-test-polygon 1) (make-point-2d)
           (make-test-polygon 1) (make-point-2d :x 1))))
    (testing "Doesn't collide"
      (ng (collide-physics-p
           (make-test-polygon 1) (make-point-2d)
           (make-test-polygon 1) (make-point-2d :x 2.1))))
    (testing "Doesn't collide in default, but collides when a polygon rotates"
      (let ((dist (* (sqrt 3) 2.1)))
        (ng (collide-physics-p
             (make-test-polygon 2) (make-point-2d)
             (make-test-polygon 2) (make-point-2d :y dist)))
        (ok (collide-physics-p
             (make-test-polygon 2) (make-point-2d)
             (make-test-polygon 2) (make-point-2d :y dist :angle (/ PI 6))))))))

;; Note: make-physic-rect is a syntax sugar of make-physic-polygon.
;;       So do only few test.
(deftest.ps+ rect
  (testing "Should collide"
    (ok (collide-physics-p
         (make-physic-rect :x 0 :y 0 :width 10 :height 20) (make-point-2d)
         (make-physic-rect :x 5 :y 5 :width 20 :height 10) (make-point-2d))))
  (testing "Shouldn't collide"
    (ng (collide-physics-p
         (make-physic-rect :x 0 :y 0 :width 10 :height 20) (make-point-2d)
         (make-physic-rect :x 50 :y 50 :width 20 :height 10) (make-point-2d))))
  (testing "Check other-keys (Cf. &allow-other-keys)"
    (let* ((rect (make-physic-rect :x 0 :y 0 :width 1 :height 1
                                   :target-tags '(:a :b)))
           (tags (physic-2d-target-tags rect)))
      (ok (find :a tags))
      (ok (find :b tags))
      (ng (find :not-exist tags)))))

(import 'cl-web-2d-game/physics/collision::col-two-bounding-box-p)

(deftest.ps+ for-bounding-box-2d
  (testing "Test col-two-bounding-box-p"
    (let ((target-box (make-bounding-box-2d
                       :left -1 :right 2 :bottom -3 :top 4)))
      (flet ((test (left right bottom top)
               (col-two-bounding-box-p
                target-box (make-bounding-box-2d
                            :left left :right right :bottom bottom :top top))))
        (ng (test -5 -1.1 -3 4))      ; too left
        (ng (test 2.1 5 -3 4))          ; too right
        (ng (test -1 2 -5 -3.1))      ; too bottom
        (ng (test -1 2 4.1 5))      ; too top
        (ok (test -2 3 -4 5))       ; include
        (ok (test -0.5 2.5 -2.5 4.5))   ; cross
        )))
  (testing "Test update-bounding-box"
    (let ((coordinate (make-point-2d :x 1 :y 0 :angle (/ PI 4)))
          (offset (make-point-2d :x (sqrt 2)
                                 :y (sqrt 2)
                                 :angle (/ PI 4))))
      ;; The offset on the coordinate should be (x = 1, y = 2, angle = (/ PI 2))
      (let ((global-point (transformf-point (clone-point-2d offset) coordinate)))
        (ok (within-length (point-2d-x global-point) 1))
        (ok (within-length (point-2d-y global-point) 2))
        (ok (within-angle (point-2d-angle global-point) (/ PI 2))))
      (flet ((test (physic left right bottom top)
               (update-bounding-box physic coordinate)
               (let ((tolerance 0.001)
                     (box (physic-2d-bounding-box physic)))
                 (ok (within-length (bounding-box-2d-left box) left))
                 (ok (within-length (bounding-box-2d-right box) right))
                 (ok (within-length (bounding-box-2d-bottom box) bottom))
                 (ok (within-length (bounding-box-2d-top box) top)))))
        (test (make-physic-circle :r 1 :offset offset)
              0 2 1 3)
        ;; An isosceles triangle. The bottom line is on the x axis,
        ;; and the center of the line is on the origin.
        (test (make-physic-polygon
               :pnt-list (list (make-point-2d :x -1 :y 0)
                               (make-point-2d :x  1 :y 0)
                               (make-point-2d :x  0 :y 2))
               :offset offset)
              -1 1 1 3)))))
