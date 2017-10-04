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

(plan 2)

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

(finalize)
