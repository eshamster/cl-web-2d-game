(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-collision
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-collision)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defun.ps+ add-mouse-pointer ()
  (let ((pointer (make-ecs-entity))
        (point (make-point-2d))
        (r 30))
    (add-ecs-component-list
     pointer
     point
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (with-slots (x y) point
                               (setf x (get-mouse-x))
                               (setf y (get-mouse-y)))))
     (make-physic-circle :r r))
    (add-ecs-entity pointer)))

(defstruct.ps+ collider-generator
    tag target-tag min-r max-r
    interval
    circle-p num-vertices
    up-to-down-p)

(defvar.ps+ *generator-from-up*
    (make-collider-generator
     :tag :a :target-tag :b
     :min-r 20 :max-r 40
     :interval 40
     :circle-p t :num-vertices 4
     :up-to-down-p t))

(defvar.ps+ *generator-from-down*
    (make-collider-generator
     :tag :b :target-tag :a
     :min-r 20 :max-r 40
     :interval 40
     :circle-p t :num-vertices 4
     :up-to-down-p nil))

(defvar.ps+ *collider-speed* 3)

;; The following should be reported and fixed.
;; (ps:ps (random 1.0))
;;  -> "Math.floor(1.0 * Math.random());"
(defun.ps random1 () (random))
(defun random1 () (random 1.0))

(defun.ps+ make-a-test-physic (generator r on-collision)
  (with-slots (target-tag circle-p num-vertices)
      generator
    (if circle-p
        (make-physic-circle :r r
                            :target-tags (list target-tag)
                            :on-collision on-collision)
        (make-physic-polygon
         :pnt-list (loop for i from 0 below num-vertices
                      collect (let ((angle (/ (* 2 PI i) num-vertices)))
                                (make-point-2d :x (* r (cos angle))
                                               :y (* r (sin angle)))))
         :target-tags (list target-tag)
         :on-collision on-collision))))

(defun.ps+ add-a-collider (generator)
  (with-slots (min-r max-r up-to-down-p
                     tag target-tag
                     circle-p num-vertices)
      generator
    (let* ((entity (make-ecs-entity))
           (r (+ min-r (* max-r (random1))))
           (color #x888888)
           (mesh (if circle-p
                     (make-solid-circle :r r :color color)
                     (make-solid-regular-polygon :r r :n num-vertices :color color)))
           (model (make-model-2d :model mesh
                                 :depth 0))
           (collide-p nil)
           (point (make-point-2d :x (* (get-screen-width) (random1))
                                 :y (if up-to-down-p
                                        (+ (get-screen-height) r)
                                        (* r -1))))
           (speed-abs *collider-speed*))
      (add-ecs-component-list
       entity
       point
       (make-a-test-physic generator r
                           (lambda (mine target)
                             (declare (ignore mine target))
                             (setf collide-p t)))
       (make-script-2d :func (lambda (entity)
                               (incf (point-2d-angle point) 0.01)
                               (with-slots (y) point
                                 (incf (point-2d-y point)
                                       (* speed-abs (if up-to-down-p -1 1)))
                                 (when (or (< y (* r -1 5))
                                           (> y (+ (get-screen-height) (* r 5))))
                                   (register-next-frame-func
                                    (lambda () (delete-ecs-entity entity)))))
                               (if collide-p
                                   (progn (setf collide-p nil)
                                          (enable-model-2d entity :target-model-2d model))
                                   (disable-model-2d entity :target-model-2d model))))
       model)
      (add-entity-tag entity tag)
      (add-ecs-entity entity))))

(defun.ps+ add-colliders ()
  (labels ((rec (generator)
             (with-slots (interval) generator
               (register-nframes-after-func
                (lambda ()
                  (add-a-collider generator)
                  (rec generator))
                interval))))
    (rec *generator-from-up*)
    (rec *generator-from-down*)))

(defun.ps+ add-gui-panels ()
  (flet ((add (folder-name generator)
           (let ((folder (add-panel-folder folder-name)))
             (with-slots (interval circle-p num-vertices) generator
               (add-panel-number "num per second" (/ 60.0 interval)
                                 :min 1 :max 60 :step 1
                                 :folder folder
                                 :on-change (lambda (value)
                                              (setf interval (floor (/ 60.0 value)))))
               (add-panel-bool "is circle" circle-p
                               :folder folder
                               :on-change (lambda (value)
                                            (setf circle-p value)))
               (add-panel-number "num of vertices" num-vertices
                                 :min 3 :max 15 :step 1
                                 :folder folder
                                 :on-change (lambda (value)
                                              (setf num-vertices value)))))))
    (add "From up" *generator-from-up*)
    (add "From down" *generator-from-down*))
  (add-panel-bool 'display-collider-model t
                  :on-change (lambda (value)
                               (setf-collider-model-enable value))))

(defun.ps+ init-func (scene)
  (init-gui)
  (init-input)
  (add-mouse-pointer)
  (add-colliders)
  (add-gui-panels)
  (init-default-systems :scene scene))

(defun.ps count-to-string (count-a count-b)
  (+ "↓: " count-a ", ↑: " count-b))

(defun.ps+ monitor-collider-count ()
  (let ((count-a 0)
        (count-b 0))
    (do-ecs-entities entity
      (cond ((has-entity-tag entity :a) (incf count-a))
            ((has-entity-tag entity :b) (incf count-b))))
    (add-to-monitoring-log (count-to-string count-a count-b))))

(defun.ps+ update-func ()
  (monitor-collider-count))
