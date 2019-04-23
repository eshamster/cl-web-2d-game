(defpackage cl-web-2d-game/t/basic-generator
  (:use :cl
        :rove
        :ps-experiment/t/test-utils
        :cl-web-2d-game/utils/basic-generator
        :cl-web-2d-game/utils/stage-generator)
  (:import-from :cl-web-2d-game/core/basic-components
                :make-point-2d
                :make-speed-2d)
  (:import-from :cl-web-2d-game/t/test-utils
                :is-point
                :is-vector)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defparameter.ps+))
(in-package :cl-web-2d-game/t/basic-generator)

(defparameter.ps+ *buf* nil)

(defmacro.ps+ process-interpreter (def)
  `(let ((test-stage (generate-stage ,def)))
     (process-stage test-stage)
     *buf*))

;; --- test --- ;;

(def-stage-element-interpreter.ps+
    (:point-tester (:include :point)) ()
  (setf *buf* point))

(deftest.ps+ for-point-component
  (ok (is-point (process-interpreter
                 (:point-tester :time 0 (:point :x 1 :y 2 :angle PI)))
                1 2 PI)))

(def-stage-element-interpreter.ps+
    (:speed-tester (:include :speed)) ()
  (setf *buf* speed))

(deftest.ps+ for-speed-component
  (ok (is-vector (process-interpreter
                  (:speed-tester :time 0 (:speed :x 1 :y 2)))
                 1 2)))
