(in-package :cl-user)
(defpackage cl-web-2d-game/physics/collision
  (:use :cl
        :parenscript
        :cl-ps-ecs
        :ps-experiment
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/utils/calc)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair)
  (:export :collide-entities-p
           :collide-physics-p

           :bounding-box-2d
           :make-bounding-box-2d
           :bounding-box-2d-left
           :bounding-box-2d-right
           :bounding-box-2d-bottom
           :bounding-box-2d-top
           :col-two-bounding-box-p

           :physic-2d
           :make-physic-2d
           :physic-2d-offset
           :physic-2d-bounding-box
           :physic-2d-target-tags
           :on-collision

           :physic-circle
           :make-physic-circle
           :physic-circle-r

           :physic-polygon
           :make-physic-polygon
           :physic-polygon-pnt-list
           :make-physic-rect

           :update-bounding-box
           :judge-collision-target-tags))
(in-package :cl-web-2d-game/physics/collision)

#|
Note: col-xx-vec takes 'point' and 'offset' for each physic. The 'point' means a global point of an entity that has the physic. Then, the 'offset' means a local offset in the entity.
|#

;; --- components --- ;;

(defstruct.ps+ bounding-box-2d
    (left -100000) (right 100000)
    (bottom -100000) (top 100000))

;; Note: The bounding box is calculated in global coordinate.
(defstruct.ps+ (physic-2d (:include ecs-component))
  kind
  (offset (make-point-2d))
  (target-tags '())
  (bounding-box (make-bounding-box-2d))
  (on-collision (lambda (mine target) (declare (ignore mine target)) nil)))

(defstruct.ps+ (physic-circle (:include physic-2d (kind :circle))) (r 0))

(defstruct.ps+ (physic-polygon (:include physic-2d (kind :polygon)))
  (pnt-list '()))

(defun.ps+ make-physic-rect (&rest rest &key (x 0) (y 0) (width 0) (height 0) &allow-other-keys)
  (dolist (key (list :x :y :width :height))
    (remf rest key))
  (let ((pnt-list (list (make-point-2d :x x           :y y)
                        (make-point-2d :x (+ x width) :y y)
                        (make-point-2d :x (+ x width) :y (+ y height))
                        (make-point-2d :x x           :y (+ y height)))))
    (apply #'make-physic-polygon
           (list* :pnt-list pnt-list
                  rest))))

;; --- basic funcions --- ;;

;; c: Circle
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
    (when (is-pnt-in-polygon cx cy point-list-po)
      (return-from col-cp t))
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
        (global-point-list-po (make-global-point-list
                               point-po offset-po vec-list-po)))
    (transformf-point global-point-c point-c)
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

;; - p (po) to p (po) - ;;

;; Algorithm: Two polygons collision under following conditions
;; 1. Any of line of each intersects. <OR>
;; 2. A point in a polygon is in the other polygon.
(defun.ps+ col-pp (point-list1 point-list2)
  "Do collision between two convec polygons."
  (let ((len1 (length point-list1))
        (len2 (length point-list2)))
    ;; Check condition 1
    (dotimes (i len1)
      (let ((pnt1-1 (nth i point-list1))
            (pnt1-2 (nth (mod (1+ i) len1) point-list1)))
        (dotimes (j len2)
          (let ((pnt2-1 (nth j point-list2))
                (pnt2-2 (nth (mod (1+ j) len2) point-list2)))
            (when (intersects-two-lines
                   (vector-2d-x pnt1-1) (vector-2d-y pnt1-1)
                   (vector-2d-x pnt1-2) (vector-2d-y pnt1-2)
                   (vector-2d-x pnt2-1) (vector-2d-y pnt2-1)
                   (vector-2d-x pnt2-2) (vector-2d-y pnt2-2))
              (return-from  col-pp t))))))
    ;; Check condition2
    (let ((pnt1 (nth 0 point-list1))
          (pnt2 (nth 0 point-list2)))
      (or (is-pnt-in-polygon (vector-2d-x pnt1) (vector-2d-y pnt1) point-list2)
          (is-pnt-in-polygon (vector-2d-x pnt2) (vector-2d-y pnt2) point-list1)))))

(defun.ps+ col-pp-vec (point1 offset1 point-list1 point2 offset2 point-list2)
  (col-pp (make-global-point-list point1 offset1 point-list1)
          (make-global-point-list point2 offset2 point-list2)))

(defun.ps+ col-pp-physic (polygon1 point1 polygon2 point2)
  (check-type polygon1 physic-polygon)
  (check-type polygon2 physic-polygon)
  (col-pp-vec point1 (physic-2d-offset polygon1) (physic-polygon-pnt-list polygon1)
              point2 (physic-2d-offset polygon2) (physic-polygon-pnt-list polygon2)))

;; --- auxiliary functions --- ;;

(defun.ps+ intersects-line-and-circle (cx cy cr lx1 ly1 lx2 ly2)
  ;; TODO: Reduce memory allocations
  ;; TODO: Compare distance using distance^2
  (> cr
     (abs (calc-dist-to-line-seg (make-vector-2d :x cx :y cy)
                                 (make-vector-2d :x lx1 :y ly1)
                                 (make-vector-2d :x lx2 :y ly2)))))

(defun.ps+ intersects-two-lines (l1x1 l1y1 l1x2 l1y2
                                 l2x1 l2y1 l2x2 l2y2)
  (flet ((calc-to-line1 (x y)
           (- (* (- l1y2 l1y1) (- x l1x1))
              (* (- l1x2 l1x1) (- y l1y1))))
         (calc-to-line2 (x y)
           (- (* (- l2y2 l2y1) (- x l2x1))
              (* (- l2x2 l2x1) (- y l2y1)))))
    (and (> 0 (* (calc-to-line1 l2x1 l2y1) (calc-to-line1 l2x2 l2y2)))
         (> 0 (* (calc-to-line2 l1x1 l1y1) (calc-to-line2 l1x2 l1y2))))))

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

(defun.ps+ is-pnt-in-polygon (target-x target-y point-list-po)
  (dotimes (i (- (length point-list-po) 2))
    (when (is-pnt-in-triangle
           target-x target-y
           (vector-2d-x (nth 0 point-list-po))
           (vector-2d-y (nth 0 point-list-po))
           (vector-2d-x (nth (+ i 1) point-list-po))
           (vector-2d-y (nth (+ i 1) point-list-po))
           (vector-2d-x (nth (+ i 2) point-list-po))
           (vector-2d-y (nth (+ i 2) point-list-po)))
      (return-from is-pnt-in-polygon t))))

(defun.ps+ make-global-point-list (coordinate offset vec-list)
  ;; TODO: Reduce memory allocations
  (let ((global-point-list '()))
    (dolist (vec vec-list)
      (let ((point (clone-point-2d offset)))
        (incf-vector-2d point vec)
        (transformf-point point coordinate)
        (push point global-point-list)))
    global-point-list))

;; --- for bounding-box --- ;;

(defun.ps+ update-bounding-box (physic global-coordinate)
  "Update bounding box in the physic in global coordinate"
  (with-slots (bounding-box offset) physic
    (with-slots (left right bottom top) bounding-box
      (let ((global-point (clone-point-2d offset)))
        (transformf-point global-point global-coordinate)
        (etypecase physic
          (physic-circle
           (with-slots-pair ((r) physic
                             ((gx x) (gy y)) global-point)
             (setf left   (- gx r) right (+ gx r)
                   bottom (- gy r) top   (+ gy r))))
          (physic-polygon
           (let ((buffer-pnt (make-point-2d))
                 (initialized-p nil))
             (dolist (pnt (physic-polygon-pnt-list physic))
               (copy-point-2d-to buffer-pnt pnt)
               (transformf-point buffer-pnt global-point)
               (with-slots (x y) buffer-pnt
                 (when (or (not initialized-p) (< x left))
                   (setf left x))
                 (when (or (not initialized-p) (> x right))
                   (setf right x))
                 (when (or (not initialized-p) (< y bottom))
                   (setf bottom y))
                 (when (or (not initialized-p) (> y top))
                   (setf top y)))
               (setf initialized-p t))))))))
  (physic-2d-bounding-box physic))

(defun.ps+ col-two-bounding-box-p (box1 box2)
  (with-slots-pair (((left1 left) (right1 right) (bottom1 bottom) (top1 top)) box1
                    ((left2 left) (right2 right) (bottom2 bottom) (top2 top)) box2)
    (not (or (> left1 right2) (> left2 right1)
             (> bottom1 top2) (> bottom2 top1)))))

;; --- main functions --- ;;

(defun.ps+ collide-physics-p (ph1 pnt1 ph2 pnt2)
  (unless (col-two-bounding-box-p
           (physic-2d-bounding-box ph1)
           (physic-2d-bounding-box ph2))
    (return-from collide-physics-p nil))
  (labels ((is-kind-pair (physic1 physic2 kind1 kind2)
             (and (eq (physic-2d-kind physic1) kind1)
                  (eq (physic-2d-kind physic2) kind2))))
    (cond ((is-kind-pair ph1 ph2 :circle :circle)
           (col-cc-physic ph1 pnt1 ph2 pnt2))
          ((is-kind-pair ph1 ph2 :circle :polygon)
           (col-cp-physic ph1 pnt1 ph2 pnt2))
          ((is-kind-pair ph1 ph2 :polygon :circle)
           (col-cp-physic ph2 pnt2 ph1 pnt1))
          ((is-kind-pair ph1 ph2 :polygon :polygon)
           (col-pp-physic ph1 pnt1 ph2 pnt2))
          (t (error "not recognized physical type")))))

(defun.ps+ collide-entities-with-physics-p (entity1 ph1 entity2 ph2)
  (let ((pnt1 (calc-global-point entity1))
        (pnt2 (calc-global-point entity2)))
    (collide-physics-p ph1 pnt1 ph2 pnt2)))

(defun.ps+ collide-entities-p (entity1 entity2)
  (with-ecs-components ((ph1 physic-2d)) entity1
    (with-ecs-components ((ph2 physic-2d)) entity2
      (collide-entities-with-physics-p entity1 ph1 entity2 ph2))))

(defun.ps+ judge-collision-target-tags (entity1 ph1 entity2 ph2)
  "The collision should be done in the following cases: 1. Both physics has no target. 2. One of each physics has a targeted tag, it should be "
  (check-type ph1 physic-2d)
  (check-type ph2 physic-2d)
  (when (and (= (length (physic-2d-target-tags ph1)) 0)
             (= (length (physic-2d-target-tags ph2)) 0))
    (return-from judge-collision-target-tags t))
  (dolist (tag (physic-2d-target-tags ph1))
    (when (has-entity-tag entity2 tag)
      (return-from judge-collision-target-tags t)))
  (dolist (tag (physic-2d-target-tags ph2))
    (when (has-entity-tag entity1 tag)
      (return-from judge-collision-target-tags t)))
  nil)

