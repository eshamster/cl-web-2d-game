(in-package :cl-user)
(defpackage cl-web-2d-game/utils/debug/debug-drawer
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/2d-geometry
        :cl-web-2d-game/graphics/draw-model-system
        :parenscript)
  (:export :draw-debug-point
           :draw-debug-point-by-time
           :draw-debug-line
           :draw-debug-line-by-time
           :*standard-debug-point-r*
           :*standard-debug-color*
           :*standard-debug-depth*))
(in-package :cl-web-2d-game/utils/debug/debug-drawer)

(enable-ps-experiment-syntax)

;; --- Draw debug models --- ;;

;; - Basic - ;;

(defun.ps+ draw-debug-model (&key model point
                                  (tag-list '())
                                  (parent nil)
                                  fn-delete-condition)
  (check-type model model-2d)
  (check-type point vector-2d)
  (when parent
    (check-type parent ecs-entity))
  (let ((entity (make-ecs-entity))
        (true-point (if (typep point 'point-2d)
                        point
                        (make-point-2d :x (vector-2d-x point)
                                       :y (vector-2d-y point)))))
    (dolist (tag tag-list)
      (add-entity-tag entity tag))
    (add-ecs-component-list
     entity true-point model
     (make-script-2d
      :func (lambda (entity)
              (when (funcall fn-delete-condition entity)
                (register-next-frame-func
                 (lambda () (delete-ecs-entity entity)))))))
    (register-next-frame-func
     (lambda () (add-ecs-entity entity parent)))))

(defvar.ps+ *standard-debug-color* #xff0000)
(defvar.ps+ *standard-debug-depth* 100)

;; -- Point -- ;;

(defvar.ps+ *standard-debug-point-r* 4)

(defun.ps+ draw-debug-point (&key point
                                  (tag-list '())
                                  (parent nil)
                                  (r *standard-debug-point-r*)
                                  fn-delete-condition)
  (draw-debug-model
   :model (make-model-2d :model (make-wired-regular-polygon
                                 :n 60 :r r
                                 :color *standard-debug-color*)
                         :depth *standard-debug-depth*)
   :point point
   :tag-list tag-list
   :parent parent
   :fn-delete-condition fn-delete-condition))

(defun.ps+ draw-debug-point-by-time (&key point
                                          (tag-list '())
                                          (parent nil)
                                          (r *standard-debug-point-r*)
                                          (time 60))
  (draw-debug-point
   :point point
   :tag-list tag-list
   :parent parent
   :r r
   :fn-delete-condition (make-fn-timer-condition time)))

;; - Line - ;;

(defun.ps+ draw-debug-line (&key point1 point2
                                 (tag-list '())
                                 (parent nil)
                                 fn-delete-condition)
  (draw-debug-model
   :model (make-model-2d :model (make-line
                                 :pos-a (list (vector-2d-x point1)
                                              (vector-2d-y point1))
                                 :pos-b (list (vector-2d-x point2)
                                              (vector-2d-y point2))
                                 :color *standard-debug-color*)
                         :depth *standard-debug-depth*)
   :point (make-point-2d)
   :tag-list tag-list
   :parent parent
   :fn-delete-condition fn-delete-condition))

(defun.ps+ draw-debug-line-by-time (&key point1 point2
                                         (tag-list '())
                                         (parent nil)
                                         (time 60))
  (draw-debug-line
   :point1 point1
   :point2 point2
   :tag-list tag-list
   :parent parent
   :fn-delete-condition (make-fn-timer-condition time)))

;; --- tools --- ;;

(defun.ps+ make-fn-timer-condition (time)
  (lambda (entity)
    (declare (ignore entity))
    (decf time)
    (<= time 0)))
