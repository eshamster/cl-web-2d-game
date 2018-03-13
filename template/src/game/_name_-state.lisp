(defpackage {{name}}/src/game/{{name}}-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-{{name}}-start-state))
(in-package :{{name}}/src/game/{{name}}-state)

(defstruct.ps+
    ({{name}}-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  nil)))))

(defstruct.ps+
    ({{name}}-start-state
     (:include game-state
               (start-process
                (lambda (_this)
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (make-{{name}}-main-state)))
               (end-process
                (lambda (_this)
                  t)))))
