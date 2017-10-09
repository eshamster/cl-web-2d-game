(in-package :cl-user)
(defpackage cl-web-2d-game-test.collision
  (:use :cl
        :prove
        :cl-ps-ecs
        :cl-web-2d-game-test.test-utils)
  (:import-from :ps-experiment
                :defun.ps+)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both))
(in-package :cl-web-2d-game-test.collision)

;; --- prepare --- ;;

(use-packages-for-test :collision)

;; --- test --- ;;

(plan 4)

(defun.ps+ same-bool-p (a b)
  "This is required because 'false' and 'null' is not same in JavaScript."
  (when (or (and a b)
            (and (not a) (not b)))
    t))

(subtest "Circle to Circle"
  (with-prove-in-both ()
    (labels ((test-2-circles (expected r1 x1 y1
                                       r2 x2 y2)
               (is (collide-physics-p
                    (make-physic-circle :r r1) (make-point-2d :x x1 :y y1)
                    (make-physic-circle :r r2) (make-point-2d :x x2 :y y2))
                   expected
                   :test #'same-bool-p)))
      (print "A circle includes another circle")
      (test-2-circles t 2 0 0 1 0 0)
      (print "Circles cross")
      (test-2-circles t 2 1 0 1 0 0)
      (print "doesn't collide")
      (test-2-circles nil 1 -10 1 3 1 1))))

(subtest "Circle to Polygon"
  (with-prove-in-both ()
    ;; Use a regular hexagon as a polygon
    (let ((polygon (make-physic-polygon
                    :pnt-list (list (make-point-2d :x  2 :y 0)
                                    (make-point-2d :x  1 :y (sqrt 3))
                                    (make-point-2d :x -1 :y (sqrt 3))
                                    (make-point-2d :x -2 :y 0)
                                    (make-point-2d :x -1 :y (* -1 (sqrt 3)))
                                    (make-point-2d :x  1 :y (* -1 (sqrt 3)))))))
      (labels ((test-cp (expected rc xc yc
                                  pnt-polygon)
                 (is (collide-physics-p
                      (make-physic-circle :r rc) (make-point-2d :x xc :y yc)
                      polygon pnt-polygon)
                     expected
                     :test #'same-bool-p)))
        (print "A circle includes a polygon")
        (test-cp t 3 0 0 (make-point-2d))
        (print "A circle is included in a polygon")
        (test-cp t 1 0 0 (make-point-2d))
        (print "A circle crosses to a polygon")
        (test-cp t 1 2 0 (make-point-2d))
        (print "Doesn't collide in default, but collides when a polygon rotates")
        (labels ((test-rotate (expected angle)
                   (test-cp expected 1.05 0 3
                            (make-point-2d :angle angle))))
          (test-rotate nil 0)
          (test-rotate t (/ PI 6)))))))

(subtest "Polygon to Polygon"
  (with-prove-in-both ()
    ;; Use a regular hexagon as a polygon
    (flet ((make-test-polygon (r)
             (let ((pnt-list (list (make-point-2d :x  2 :y 0)
                                   (make-point-2d :x  1 :y (sqrt 3))
                                   (make-point-2d :x -1 :y (sqrt 3))
                                   (make-point-2d :x -2 :y 0)
                                   (make-point-2d :x -1 :y (* -1 (sqrt 3)))
                                   (make-point-2d :x  1 :y (* -1 (sqrt 3))))))
               (dolist (pnt pnt-list)
                 (setf-vector-abs pnt r))
               (make-physic-polygon :pnt-list pnt-list))))
      (print "A polygon includes another")
      (ok (collide-physics-p
           (make-test-polygon 2) (make-point-2d)
           (make-test-polygon 1) (make-point-2d)))
      (print "Two polygons cross")
      (ok (collide-physics-p
           (make-test-polygon 1) (make-point-2d)
           (make-test-polygon 1) (make-point-2d :x 1)))
      (print "Doesn't collide")
      (ok (not (collide-physics-p
                (make-test-polygon 1) (make-point-2d)
                (make-test-polygon 1) (make-point-2d :x 2.1))))
      (print "Doesn't collide in default, but collides when a polygon rotates")
      (let ((dist (* (sqrt 3) 2.1)))
        (ok (not (collide-physics-p
                  (make-test-polygon 2) (make-point-2d)
                  (make-test-polygon 2) (make-point-2d :y dist))))
        (ok (collide-physics-p
             (make-test-polygon 2) (make-point-2d)
             (make-test-polygon 2) (make-point-2d :y dist :angle (/ PI 6))))))))

(import 'cl-web-2d-game.collision::col-two-bounding-box-p)

(subtest "Test bounding-box-2d"
  (subtest "Test col-two-bounding-box-p"
    (with-prove-in-both ()
      (let ((target-box (make-bounding-box-2d
                         :left -1 :right 2 :bottom -3 :top 4)))
        (flet ((test (left right bottom top expected)
                 (is (col-two-bounding-box-p
                      target-box (make-bounding-box-2d
                                  :left left :right right :bottom bottom :top top))
                     expected
                     :test #'same-bool-p)))
          (test -5 -1.1 -3 4 nil) ; too left
          (test 2.1 5 -3 4 nil) ; too right
          (test -1 2 -5 -3.1 nil) ; too bottom
          (test -1 2 4.1 5 nil) ; too top
          (test -2 3 -4 5 t) ; include
          (test -0.5 2.5 -2.5 4.5 t) ; cross
          ))))
  (subtest "Test update-bounding-box"
    (with-prove-in-both ()
      (let ((coordinate (make-point-2d :x 1 :y 0 :angle (/ PI 4)))
            (offset (make-point-2d :x (sqrt 2)
                                   :y (sqrt 2)
                                   :angle (/ PI 4))))
        ;; The offset on the coordinate should be (x = 1, y = 2, angle = (/ PI 2))
        (let ((global-point (transformf-point (clone-point-2d offset) coordinate))
              (tolerance 0.001))
          (within (point-2d-x global-point) 1 tolerance)
          (within (point-2d-y global-point) 2 tolerance)
          (within (point-2d-angle global-point) (/ PI 2) tolerance))
        (flet ((test (physic left right bottom top)
                 (update-bounding-box physic coordinate)
                 (let ((tolerance 0.001)
                       (box (physic-2d-bounding-box physic)))
                   (within (bounding-box-2d-left box) left tolerance)
                   (within (bounding-box-2d-right box) right tolerance)
                   (within (bounding-box-2d-bottom box) bottom tolerance)
                   (within (bounding-box-2d-top box) top tolerance))))
          (test (make-physic-circle :r 1 :offset offset)
                0 2 1 3)
          ;; An isosceles triangle. The bottom line is on the x axis,
          ;; and the center of the line is on the origin.
          (test (make-physic-polygon
                 :pnt-list (list (make-point-2d :x -1 :y 0)
                                 (make-point-2d :x  1 :y 0)
                                 (make-point-2d :x  0 :y 2))
                 :offset offset)
                -1 1 1 3))))))

(finalize)
