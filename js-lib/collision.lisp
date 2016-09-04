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

           :physic-2d
           :make-physic-2d

           :physic-circle
           :make-physic-circle

           :physic-triangle
           :make-physic-triangle

           :collision-system
           :make-collision-system))
(in-package :cl-web-2d-game.collision)

;; --- components --- ;;

(defstruct.ps+ (physic-2d (:include ecs-component))
  kind
  (offset (make-vector-2d))
  (on-collision (lambda (mine target) (declare (ignore mine target)) nil)))

(defstruct.ps+ (physic-circle (:include physic-2d (kind :circle))) (r 0))

(defstruct.ps+ (physic-triangle (:include physic-2d (kind :triangle)))
    (pnt1 (make-vector-2d)) (pnt2 (make-vector-2d)) (pnt3 (make-vector-2d)))

;; --- basic funcions --- ;;

;; c: Circle
;; t: Triangle
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

;; - c to t - ;;

(defun.ps+ col-ct (cx cy cr
                   tx1 ty1 tx2 ty2 tx3 ty3)
  ;; A circle and a triangle collisions under following conditions
  ;;   1. The center of the circle is in the triangle. <OR>
  ;;   2. A line of the triangle intersects to the circle.
  ;; Note: The second condition can't check only the case where
  ;;       any point of the circle is in the triangle. The first
  ;;       condition can check such case.
  (or (is-pnt-in-triangle cx cy tx1 ty1 tx2 ty2 tx3 ty3)
      (intersects-line-and-circle cx cy cr tx1 ty1 tx2 ty2)
      (intersects-line-and-circle cx cy cr tx2 ty2 tx3 ty3)
      (intersects-line-and-circle cx cy cr tx3 ty3 tx1 ty1)))

;; Note: the offset parameters is not well-tested
(defun.ps+ col-ct-vec (point-c offset-c rc
                       point-t offset-t vertex-t1 vertex-t2 vertex-t3)
  (macrolet ((add-x (&rest point-list)
               `(+ ,@(mapcar (lambda (point) `(vector-2d-x ,point)) point-list)))
             (add-y (&rest point-list)
               `(+ ,@(mapcar (lambda (point) `(vector-2d-y ,point)) point-list))))
    (col-ct (add-x point-c offset-c)
            (add-y point-c offset-c)
            rc
            (add-x vertex-t1 offset-t point-t)
            (add-y vertex-t1 offset-t point-t)
            (add-x vertex-t2 offset-t point-t)
            (add-y vertex-t2 offset-t point-t)
            (add-x vertex-t3 offset-t point-t)
            (add-y vertex-t3 offset-t point-t))))

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

(defun.ps+ process-collision (entity1 entity2)
  (with-ecs-components ((ph1 physic-2d) (pnt1 point-2d)) entity1
    (with-ecs-components ((ph2 physic-2d) (pnt2 point-2d)) entity2
      (labels ((is-kind-pair (physic1 physic2 kind1 kind2)
                 (and (eq (physic-2d-kind physic1) kind1)
                      (eq (physic-2d-kind physic2) kind2))))
        (when (cond ((is-kind-pair ph1 ph2 :circle :circle)
                     (col-cc-physic ph1 pnt1 ph2 pnt2))
                    ((is-kind-pair ph1 ph2 :circle :triangle)
                     (col-ct-physic ph1 pnt1 ph2 pnt2))
                    ((is-kind-pair ph1 ph2 :triangle :circle)
                     (col-ct-physic ph2 pnt2 ph1 pnt1))
                    (t (error "not recognized physical type")))
          (with-slots-pair (((event1 on-collision)) ph1
                            ((event2 on-collision)) ph2)
            (funcall event1 entity1 entity2)
            (funcall event2 entity2 entity1)))))))

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
