(defpackage cl-web-2d-game/t/stage-generator
  (:use :cl
        :rove
        :ps-experiment/t/test-utils
        :cl-web-2d-game/utils/stage-generator)
  (:import-from :ps-experiment
                :defparameter.ps+))
(in-package :cl-web-2d-game/t/stage-generator)

;; --- test --- ;;

(deftest.ps+ main
  (let* ((buffer 0)
         (test-stage (generate-stage
                       (:general :time 5 :func (lambda () (setf buffer 5)))
                       (:general :time 2 :func (lambda () (setf buffer 2)))
                       (dotimes (i 2)
                         (let ((time (* (1+ i) 3)))
                           (stage (:general :time time
                                            :func (lambda () (setf buffer time))))))))
         ;; ((time buffer) ...)
         (expected-buffer '((0 0)
                            (1 0)
                            (2 2)
                            (3 3)
                            (4 3)
                            (5 5)
                            (6 6)
                            (7 6))))
    (dolist (time-and-value expected-buffer)
      (let ((expected (cadr time-and-value)))
        (process-stage test-stage)
        (ok (= buffer expected))))))

;; TODO: clean up environment after testing

(defparameter.ps+ *test-value* 0)

(def-stage-element-interpreter.ps+ :hoge (value)
  (setf *test-value* value))

(deftest.ps+ for-def-stage-element-interpreter
  (let* ((test-stage (generate-stage
                       (:hoge :time 5 :value 5)
                       (:hoge :time 2 :value 2)
                       (dotimes (i 2)
                         (let ((time (* (1+ i) 3)))
                           (stage (:hoge :time time :value time))))))
         ;; ((time buffer) ...)
         (expected-buffer '((0 0)
                            (1 0)
                            (2 2)
                            (3 3)
                            (4 3)
                            (5 5)
                            (6 6)
                            (7 6))))
    (dolist (time-and-value expected-buffer)
      (let ((expected (cadr time-and-value)))
        (process-stage test-stage)
        (ok (= *test-value* expected))))))

(def-stage-element-interpreter.ps+ :hoge-included (value1)
  value1)

(def-stage-element-interpreter.ps+
    (:hoge-including (:include :hoge-included)) (value2)
  (setf *test-value* (+ value2 hoge-included)))

(deftest.ps+ for-including-other-interpreter
  (setf *test-value* 0)
  (ok (= *test-value* 0))
  (let* ((test-stage (generate-stage
                       (:hoge-including :time 0 :value2 100
                                        (:hoge-included :value1 200)))))
    (process-stage test-stage)
    (ok (= *test-value* 300))))
