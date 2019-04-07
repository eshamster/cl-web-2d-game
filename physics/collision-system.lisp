(in-package :cl-user)
(defpackage cl-web-2d-game/physics/collision-system
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/2d-geometry
        :cl-web-2d-game/graphics/draw-model-system
        :cl-web-2d-game/utils/calc
        :cl-web-2d-game/utils/debug/performance)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair)
  (:export :collision-system
           :make-collision-system
           :setf-collider-model-enable
           :setf-collider-model-depth
           :setf-collider-model-color))
(in-package :cl-web-2d-game/physics/collision-system)

(enable-ps-experiment-syntax)

;; --- collider model --- ;;

(defvar.ps+ *collider-model-color* #x00ff00)
(defvar.ps+ *collider-model-depth* 10)
(defvar.ps+ *collider-model-enable* t)

(defun.ps+ generate-collider-model (physic-2d)
  (flet ((make-a-model (fn-make-geometry)
           (make-model-2d :model (funcall fn-make-geometry *collider-model-color*)
                          :depth *collider-model-depth*
                          :offset (clone-point-2d (physic-2d-offset physic-2d)))))
    (etypecase physic-2d
      (physic-circle (make-a-model
                      (lambda (color)
                        (make-wired-circle
                         :color color
                         :r (physic-circle-r physic-2d)))))
      (physic-polygon (make-a-model
                       (lambda (color)
                         (make-wired-polygon
                          :color color
                          :pnt-list (mapcar (lambda (pnt)
                                              (list (vector-2d-x pnt)
                                                    (vector-2d-y pnt)))
                                            (physic-polygon-pnt-list physic-2d)))))))))

(defun.ps+ add-collider-model (entity)
  (register-next-frame-func
   (lambda ()
     (with-ecs-components (physic-2d) entity
       (unless (find-collider-model physic-2d)
         (add-ecs-component (generate-collider-model physic-2d) entity
                            physic-2d))))))

(defun.ps+ find-collider-model (physic)
  "Find collider model that should be added as a child of physic"
  (find-a-component (lambda (target) (typep target 'model-2d))
                    physic))

(defun.ps+ setf-collider-model-enable (value)
  (unless (eq value *collider-model-enable*)
    (setf *collider-model-enable* value)
    (do-ecs-entities entity
      (do-ecs-components-of-entity (physic entity
                                           :component-type 'physic-2d)
        (let ((model (find-collider-model physic)))
          (if model
              (if value
                  (enable-model-2d entity :target-model-2d model)
                  (disable-model-2d entity :target-model-2d model))
              (when value
                (add-collider-model entity))))))
    t))

(defun.ps+ setf-collider-model-depth (value)
  (setf *collider-model-depth* value))

(defun.ps+ setf-collider-model-color (value)
  (setf *collider-model-color* value))

;; --- collision --- ;;

;; The with-ecs-components takes some time, so buffer
;; components before collision loop (double loop)
(defstruct.ps+ collision-entity-info
    entity global-point physic target-entity-list)

(defun.ps+ process-collision (entity1 ph1 pnt1 entity2 ph2 pnt2)
  (when (eq entity1 entity2)
    (return-from process-collision))
  (when (not (judge-collision-target-tags entity1 ph1 entity2 ph2))
    (return-from process-collision))
  (when (collide-physics-p ph1 pnt1 ph2 pnt2)
    (with-slots-pair (((event1 on-collision)) ph1
                      ((event2 on-collision)) ph2)
      (funcall event1 entity1 entity2)
      (funcall event2 entity2 entity1))))

;; - collision target cache - ;;

;; TODO: Limit cache size
;; ((<tag list> <target list>) ...)
(defstruct.ps+ collision-target-cache (cache (list)))

(defun.ps+ same-tag-list-p (tag-list-1 tag-list-2)
  (let ((len1 (length tag-list-1)))
    (unless (= len1 (length tag-list-2))
      (return-from same-tag-list-p nil))
    ;; Assume that same tag list has same order,
    ;; and substance of the list is an array in JS.
    ;; (Parenscript process both list and array in CL as array in JS)
    ;; By the latter assumption, use index accessing.
    (dotimes (i len1)
      (unless (eq (nth i tag-list-1) (nth i tag-list-2))
        (return-from same-tag-list-p nil))))
  t)

(defun.ps+ add-pair-to-cache (tag-list target-entity-list cache)
  (push (list tag-list target-entity-list)
        (collision-target-cache-cache cache)))

(defun.ps+ find-target-pair (tag-list cache)
  (find-if (lambda (pair)
             (same-tag-list-p tag-list (car pair)))
           (collision-target-cache-cache cache)))

(defun.ps+ get-target-entity-info-list (entity-info all-entity-info cache)
  (let ((tag-list (physic-2d-target-tags
                   (collision-entity-info-physic entity-info))))
    (let ((pair (find-target-pair tag-list cache)))
      (when pair
        (return-from get-target-entity-info-list (cadr pair))))
    (let ((result (list)))
      (dolist (info all-entity-info)
        (with-slots-pair (((entity1 entity) (ph1 physic)) entity-info
                          ((entity2 entity) (ph2 physic)) info)
          (when (judge-collision-target-tags entity1 ph1 entity2 ph2)
            (push info result))))
      (add-pair-to-cache tag-list result cache)
      result)))

(defun.ps+ init-target-list-of-info-list (info-list)
  (let ((cache (make-collision-target-cache)))
    (labels ((rec (info rest)
               ;; Note: An empty list is not "false" in JS.
               (when (> (length rest)0)
                 (setf (collision-entity-info-target-entity-list info)
                       (get-target-entity-info-list info rest cache))
                 (rec (car rest) (cdr rest)))))
      (rec (car info-list) (cdr info-list)))))

;; - system - ;;

(defstruct.ps+
    (collision-system
     (:include ecs-system
               (target-component-types '(point-2d physic-2d))
               (process-all
                (lambda (system)
                  (with-performance ("collision")
                    (with-slots ((entities target-entities)) system
                      (let ((info-list (list)))
                        (dolist (entity entities)
                          (let ((physic (get-ecs-component 'physic-2d entity))
                                (global-point (calc-global-point entity)))
                            (update-bounding-box physic global-point)
                            (push (make-collision-entity-info
                                   :entity entity
                                   :global-point global-point
                                   :physic physic)
                                  info-list)))
                        (init-target-list-of-info-list info-list)
                        (let ((length (length info-list)))
                          (loop for outer-idx from 0 below (1- length) do
                               (with-slots ((entity1 entity)
                                            (ph1 physic)
                                            (pnt1 global-point)
                                            (target-info-list1 target-entity-list))
                                   (nth outer-idx info-list)
                                 (dolist (info2 target-info-list1)
                                   (with-slots ((entity2 entity)
                                                (ph2 physic)
                                                (pnt2 global-point))
                                       info2
                                     (process-collision entity1 ph1 pnt1
                                                        entity2 ph2 pnt2)))))))))))
               (add-entity-hook (lambda (entity)
                                  (when *collider-model-enable*
                                    (add-collider-model entity)))))))
