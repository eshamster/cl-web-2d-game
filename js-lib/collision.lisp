(in-package :cl-user)
(defpackage cl-web-2d-game.collision
  (:use :cl
        :parenscript
        :cl-ps-ecs
        :ps-experiment
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.calc)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair)
  (:export :process-collision
           :collide-entities-p

           :physic-2d
           :make-physic-2d

           :physic-circle
           :make-physic-circle

           :physic-triangle
           :make-physic-triangle

           :physic-polygon
           :make-physic-polygon

           :collision-system
           :make-collision-system))
(in-package :cl-web-2d-game.collision)

#|
Note: col-xx-vec takes 'point' and 'offset' for each physic. The 'point' means a global point of an entity that has the physic. Then, the 'offset' means a local offset in the entity.
|#

;; --- components --- ;;

(defstruct.ps+ (physic-2d (:include ecs-component))
  kind
  (offset (make-point-2d))
  (on-collision (lambda (mine target) (declare (ignore mine target)) nil)))

(defstruct.ps+ (physic-circle (:include physic-2d (kind :circle))) (r 0))

(defstruct.ps+ (physic-triangle (:include physic-2d (kind :triangle)))
    (pnt1 (make-vector-2d)) (pnt2 (make-vector-2d)) (pnt3 (make-vector-2d)))

(defstruct.ps+ (physic-polygon (:include physic-2d (kind :polygon)))
  (pnt-list '()))

;; --- basic funcions --- ;;

;; c: Circle
;; t: Triangle
;; p: Polygon (Only convex polygon)
;; r: Rectangle

;; - c to c - ;;
(defun.ps+ col-cc (x1 y1 r1 x2 y2 r2)
  (<= (+ (expt (- x1 x2) 2)
         (expt (- y1 y2) 2))
      (expt (+ r1 r2) 2)))

;; Note: the offset parameters is not well-tested
(defun.ps+ col-cc-vec (point1 offset1 r1 point2 offset2 r2)
  (with-slots-pair (((x1 x) (y1 y)) point1
                    ((ox1 x) (oy1 y)) offset1
                    ((x2 x) (y2 y)) point2
                    ((ox2 x) (oy2 y)) offset2)
    (col-cc (+ x1 ox1) (+ y1 ox1) r1
            (+ x2 ox2) (+ y2 oy2) r2)))

(defun.ps+ col-cc-physic (circle1 point1 circle2 point2)
  (check-type circle1 physic-circle)
  (check-type circle2 physic-circle)
  (with-slots-pair (((r1 r) (offset1 offset)) circle1
                    ((r2 r) (offset2 offset)) circle2)
    (col-cc-vec point1 offset1 r1
                point2 offset2 r2)))

;; - c to p (po) - ;;
;; Use 'po' as an abbreviation of 'polygon' because 'p' is confusing with 'p' that is used for boolean values or functions.

(defun.ps+ col-cp (cx cy cr point-list-po)
  "Do collision between a circle and a convec polygon. (The cx cy cr are circle parameters. The point-list-po is a polygon parameters (A list of vector-2d)
Algorighm: A circle and a polygon collisions under following conditions
  1. The center of the circle is in the polygon. <OR>
  2. A line of the polygon intersects to the circle.
     (This contains the case where the circle includes the line.)
Note: The second condition can't check only the case where
      any point of the circle is in the polygon. The first
      condition can check such case."
  (let ((num-pnts (length point-list-po)))
    (when (= num-pnts 0)
      (error "Can't do collision with an empty poligon."))
    ;; The following pattern can be implemented, but have not.
    (when (< num-pnts 3)
      (error "Can't do collision with an point (length 1) or a line (length 2)."))
    ;; Check the center of the circle is in the polygon.
    (dotimes (i (- num-pnts 2))
      (when (is-pnt-in-triangle
             cx cy
             (vector-2d-x (nth 0 point-list-po))
             (vector-2d-y (nth 0 point-list-po))
             (vector-2d-x (nth (+ i 1) point-list-po))
             (vector-2d-y (nth (+ i 1) point-list-po))
             (vector-2d-x (nth (+ i 2) point-list-po))
             (vector-2d-y (nth (+ i 2) point-list-po)))
        (return-from col-cp t)))
    ;; Check a line of the polygon intersects to the circle.
    (dotimes (i num-pnts)
      (when (intersects-line-and-circle
             cx cy cr
             (vector-2d-x (nth i point-list-po))
             (vector-2d-y (nth i point-list-po))
             (vector-2d-x (nth (mod (1+ i) num-pnts) point-list-po))
             (vector-2d-y (nth (mod (1+ i) num-pnts) point-list-po)))
        (return-from col-cp t)))
    nil))

(defun.ps+ col-cp-vec (point-c offset-c rc point-po offset-po vec-list-po)
  ;; TODO: Reduce memory allocations
  (let ((global-point-c (clone-point-2d offset-c))
        (global-point-list-po '()))
    (transformf-point global-point-c point-c)
    (dolist (vec vec-list-po)
      (let ((point (clone-point-2d offset-po)))
        (incf-vector point vec)
        (transformf-point point point-po)
        (push point global-point-list-po)))
    (col-cp (point-2d-x global-point-c)
            (point-2d-y global-point-c)
            rc
            global-point-list-po)))

(defun.ps+ col-cp-physic (circle point-c polygon point-po)
  (check-type circle physic-circle)
  (check-type polygon physic-polygon)
  (with-slots-pair (((offset-c offset) r) circle
                    ((offset-po offset) pnt-list) polygon)
    (col-cp-vec point-c offset-c r
                point-po offset-po pnt-list)))

;; - c to t - ;;

(defun.ps+ col-ct-vec (point-c offset-c rc
                               point-t offset-t vertex-t1 vertex-t2 vertex-t3)
  ;; TODO: Reduce memory allocations
  (let ((global-point-c (clone-point-2d offset-c))
        (buffer-pnts '()))
    (transformf-point global-point-c point-c)
    (dolist (vertex (list vertex-t1 vertex-t2 vertex-t3))
        (let ((pnt (make-point-2d :x (vector-2d-x vertex) :y (vector-2d-y vertex))))
          (transformf-point pnt offset-t)
          (transformf-point pnt point-t)
          (push pnt buffer-pnts)))
    (col-cp (point-2d-x global-point-c)
            (point-2d-y global-point-c)
            rc
            buffer-pnts)))

(defun.ps+ col-ct-physic (circle point-c triangle point-t)
  (check-type circle physic-circle)
  (check-type triangle physic-triangle)
  (with-slots-pair (((offset-c offset) r) circle
                    ((offset-t offset) pnt1 pnt2 pnt3) triangle)
    (col-ct-vec point-c offset-c r
                point-t offset-t pnt1 pnt2 pnt3)))

;; --- auxiliary functions --- ;;

(defun.ps+ intersects-line-and-circle (cx cy cr lx1 ly1 lx2 ly2)
  ;; TODO: Reduce memory allocations
  ;; TODO: Compare distance using distance^2
  (> cr
     (abs (calc-dist-to-line-seg (make-vector-2d :x cx :y cy)
                                 (make-vector-2d :x lx1 :y ly1)
                                 (make-vector-2d :x lx2 :y ly2)))))

(defun.ps+ is-pnt-in-triangle (target-x target-y x1 y1 x2 y2 x3 y3)
  "Judge if a target point is in triangle or not by calculating vector product"
  (labels ((calc-vector-product (x1 y1 x2 y2)
             (- (* x1 y2) (* x2 y1)))
           (is-same-sign (a b c)
             (or (and (<= a 0) (<= b 0) (<= c 0))
                 (and (>= a 0) (>= b 0) (>= c 0)))))
    (is-same-sign
     (calc-vector-product (- x2 x1) (- y2 y1) (- target-x x1) (- target-y y1))
     (calc-vector-product (- x3 x2) (- y3 y2) (- target-x x2) (- target-y y2))
     (calc-vector-product (- x1 x3) (- y1 y3) (- target-x x3) (- target-y y3)))))

(defun.ps+ collide-entities-p (entity1 entity2)
  (with-ecs-components ((ph1 physic-2d)) entity1
    (with-ecs-components ((ph2 physic-2d)) entity2
      (labels ((is-kind-pair (physic1 physic2 kind1 kind2)
                 (and (eq (physic-2d-kind physic1) kind1)
                      (eq (physic-2d-kind physic2) kind2))))
        (let ((pnt1 (calc-global-point entity1))
              (pnt2 (calc-global-point entity2)))
          (cond ((is-kind-pair ph1 ph2 :circle :circle)
                 (col-cc-physic ph1 pnt1 ph2 pnt2))
                ((is-kind-pair ph1 ph2 :circle :triangle)
                 (col-ct-physic ph1 pnt1 ph2 pnt2))
                ((is-kind-pair ph1 ph2 :triangle :circle)
                 (col-ct-physic ph2 pnt2 ph1 pnt1))
                ((is-kind-pair ph1 ph2 :circle :polygon)
                 (col-cp-physic ph1 pnt1 ph2 pnt2))
                ((is-kind-pair ph1 ph2 :polygon :circle)
                 (col-cp-physic ph2 pnt2 ph1 pnt1))
                ;;--- TODO: Implement the followings
                ((is-kind-pair ph1 ph2 :triangle :polygon)
                 nil)
                ((is-kind-pair ph1 ph2 :polygon :triangle)
                 nil)
                ((is-kind-pair ph1 ph2 :polygon :polygon)
                 nil)
                ((is-kind-pair ph1 ph2 :triangle :triangle)
                 nil)
                (t (error "not recognized physical type"))))))))

(defun.ps+ process-collision (entity1 entity2)
  (when (collide-entities-p entity1 entity2)
    (with-ecs-components ((ph1 physic-2d)) entity1
      (with-ecs-components ((ph2 physic-2d)) entity2
        (with-slots-pair (((event1 on-collision)) ph1
                          ((event2 on-collision)) ph2)
          (funcall event1 entity1 entity2)
          (funcall event2 entity2 entity1))))))

;; --- system --- ;;

(defstruct.ps+
    (collision-system
     (:include ecs-system
               (target-component-types '(point-2d physic-2d))
               (process-all
                (lambda (system)
                  (with-slots ((entities target-entities)) system
                    (let ((length (length entities)))
                      (loop for outer-idx from 0 below (1- length) do
                           (loop for inner-idx from (1+ outer-idx) below length do
                                (process-collision (aref entities outer-idx)
                                                   (aref entities inner-idx)))))))))))
