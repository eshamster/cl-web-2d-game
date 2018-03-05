(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-text
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-text)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defvar.ps+ *text-adding-interval* 30)
(defvar.ps+ *text-color-list*
    '(("abcd2" #xff0000)
      ("test" #x00ff00)
      ("-- end --" #x0000ff)))

(defun.ps+ add-sample-text (text-area)
  (let ((lst (get-entity-param text-area :text-color-list)))
    (if (> (length lst) 0)
        (let* ((first (car lst))
               (text (car first))
               (color (cadr first)))
          (add-text-to-area text-area
                            :text text
                            :color color)
          (set-entity-param text-area :text-color-list
                            (cdr lst)))
        (progn (clear-text-area text-area)
               (set-entity-param text-area :text-color-list
                                 *text-color-list*)))))

(defun.ps+ process-adding-text (text-area)
  (let ((rest-intv (get-entity-param text-area :rest-intv)))
    (when (<= rest-intv 0)
      (add-sample-text text-area))
    (set-entity-param text-area :rest-intv
                      (if (> rest-intv 0) (1- rest-intv) *text-adding-interval*))))

(defun.ps+ add-sample-text-area (&key x y align)
  (let ((text-area (make-text-area :font-size 25 :text-align align
                                   :margin 10
                                   :x x :y y)))
    (add-ecs-component-list
     text-area
     (make-script-2d :func #'process-adding-text)
     (init-entity-params :rest-intv *text-adding-interval*
                         :text-color-list *text-color-list*))
    (add-ecs-entity text-area)))

(defun.ps+ init-func (scene)
  (set-console-log-level :debug)
  (load-font "js/")
  (add-sample-text-area :x 50 :y 400 :align :left)
  (add-sample-text-area :x 300 :y 400 :align :center)
  (add-sample-text-area :x 550 :y 400 :align :right)
  (init-default-systems :scene scene))

(defun.ps+ update-func ())
