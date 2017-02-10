(in-package :cl-user)
(defpackage cl-web-2d-game.performance
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :with-trace))
(in-package :cl-web-2d-game.performance)

(enable-ps-experiment-syntax)

;; --- performance tracer --- ;;

;; Note: this is depend on Web Tracing Framework (wtf-trace.js)

(defmacro.ps with-trace (title &body body)
  `(let ((scope (#j.WTF.trace.enterScope# ,title)))
     ,@body
     (#j.WTF.trace.leaveScope# scope ,title)))

(defmacro with-trace (title &body body)
  "(dummy)"
  (declare (ignore title))
  `(progn ,@body))
