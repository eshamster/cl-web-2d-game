(in-package :cl-user)
(defpackage cl-web-2d-game.gui
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :init-gui
           :add-panel-bool
           :add-panel-number
           :add-panel-button))
(in-package :cl-web-2d-game.gui)

(enable-ps-experiment-syntax)

(defvar.ps *gui-panel* nil)
(defvar.ps *gui-panel-params* nil)

(defun.ps init-gui ()
  (setf *gui-panel* (new (#j.dat.GUI#)))
  (setf *gui-panel-params* (make-hash-table)))

(defmacro.ps add-to-global (name init-value on-change &rest args)
  `(progn (setf (gethash ,name *gui-panel-params*) ,init-value)
          (symbol-macrolet ((adding ((@ *gui-panel* add) *gui-panel-params* ,name ,@args)))
            (if on-change
                (-- adding (on-change ,on-change))
                adding))))

(defun.ps add-panel-bool (name init-value &key (on-change nil))
  (add-to-global name init-value on-change))

(defun.ps add-panel-number (name init-value &key (on-change nil) (min -100) (max -100) (step 0.1))
  (add-to-global name init-value on-change
                 min max step))

(defun.ps add-panel-button (name &key (on-change nil))
  (add-to-global name on-change nil))
