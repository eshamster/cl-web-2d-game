(defpackage cl-web-2d-game/t/script-system
  (:use :cl
        :rove
        :cl-ps-ecs
        :ps-experiment/t/test-utils
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/core/basic-systems
        :cl-web-2d-game/t/test-utils)
  (:import-from :cl-ps-ecs/t/test-utils
                :with-ecs-env)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+))
(in-package :cl-web-2d-game/t/script-system)

;; --- utils --- ;;

(defvar.ps+ *global-counter* 0)

(defmacro.ps+ with-script-system (&body body)
  `(unwind-protect
        (with-ecs-env ()
          (register-ecs-system "script" (make-script-system))
          ,@body)
     (setf *global-counter* 0)))

(defun.ps+ process-one-frame ()
  (ecs-main))

;; --- test --- ;;

(deftest.ps+ script-system
  (testing "single script"
    (with-script-system
      (let ((entity (make-ecs-entity)))
        (add-ecs-component-list
         entity
         (make-script-2d
          :func (lambda (arg)
                  (ok (eq arg entity))
                  (incf *global-counter*))))
        (add-ecs-entity entity)
        (process-one-frame)
        (ok (= *global-counter* 1)))))
  (testing "multiple scripts"
    (with-script-system
      (let ((entity (make-ecs-entity)))
        (add-ecs-component-list
         entity
         (make-script-2d
          :func (lambda (arg)
                  (incf *global-counter*)))
         (make-script-2d
          :func (lambda (arg)
                  (incf *global-counter* 10))))
        (add-ecs-entity entity)
        (process-one-frame)
        (ok (= *global-counter* 11))))))
