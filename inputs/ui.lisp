(defpackage cl-web-2d-game/inputs/ui
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/inputs/input
        :cl-web-2d-game/physics/collision)
  (:export :init-ui-system
           :ui-component
           :make-ui-component
           :ui-component-on-mouse-hover
           :ui-component-on-mouse-not-hover
           :ui-component-on-mouse-enter
           :ui-component-on-mouse-leave
           :ui-component-on-click-up
           :ui-component-on-click-down)
  (:import-from :cl-web-2d-game/utils/calc
                :calc-global-point))
(in-package :cl-web-2d-game/inputs/ui)

(defstruct.ps+ (ui-component (:include ecs-component))
  on-click-down
  on-click-up
  on-mouse-enter
  on-mouse-leave
  on-mouse-hover
  on-mouse-not-hover
  ;; internal
  (mouse-hover-p nil))

(defvar.ps+ *current-target* nil)

(defstruct.ps+
    (ui-system
     (:include
      ecs-system
      (target-component-types '(point-2d ui-component physic-2d))
      (process
       (lambda (entity)
         (with-ecs-components (physic-2d ui-component) entity
           (let* ((mouse-pnt (make-point-2d :x (get-mouse-x)
                                            :y (get-mouse-y)))
                  (mouse-physic (make-physic-circle :r 0))
                  (collide-p (collide-physics-p physic-2d (calc-global-point entity)
                                                mouse-physic mouse-pnt)))
             ;; hover/enter/leave
             (with-slots (on-mouse-hover
                          on-mouse-not-hover
                          on-mouse-enter
                          on-mouse-leave
                          mouse-hover-p)
                 ui-component
               (if collide-p
                   (progn (when on-mouse-hover
                            (funcall on-mouse-hover))
                          (when (and on-mouse-enter (not mouse-hover-p))
                            (funcall on-mouse-enter))
                          (setf mouse-hover-p t))
                   (progn (when on-mouse-not-hover
                            (funcall on-mouse-not-hover))
                          (when (and on-mouse-leave mouse-hover-p)
                            (funcall on-mouse-leave))
                          (setf mouse-hover-p nil))))
             ;; click
             (with-slots (on-click-down on-click-up) ui-component
               (case (get-left-mouse-state)
                 (:down-now
                  (when collide-p
                    (setf *current-target* entity)
                    (when on-click-down
                      (funcall on-click-down))))
                 (:up-now
                  (when (and collide-p
                             (eq *current-target* entity)
                             (find-the-entity entity))
                    (when on-click-up
                      (funcall on-click-up)))))))))))))

(defun.ps+ init-ui-system ()
  (make-ui-system))
