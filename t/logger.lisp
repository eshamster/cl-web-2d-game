(defpackage cl-web-2d-game/t/logger
  (:use :cl
        :rove
        :cl-ps-ecs
        :ps-experiment/t/test-utils
        :cl-web-2d-game/utils/debug/logger
        :cl-web-2d-game/t/test-utils)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+))
(in-package :cl-web-2d-game/t/logger)

;; --- prepare --- ;;

(eval-when (:load-toplevel :execute :compile-toplevel)
  (defun.ps+ current-log-level ()
    'cl-web-2d-game/utils/debug/logger::*current-console-log-level*))

(defmacro.ps+ with-logger-env (() &body body)
  (with-gensyms (pre-log-function pre-log-level)
    `(let ((,pre-log-function *console-log-function*)
           (,pre-log-level ,(current-log-level)))
       (unwind-protect
            (progn ,@body)
         (progn (setf *console-log-function* ,pre-log-function)
                (setf ,(current-log-level) ,pre-log-level))))))

;; --- test --- ;;

(deftest.ps+ for-console-logger
  (testing "Decide if log is output by log level"
    (with-logger-env ()
      (let ((result nil))
        (setf *console-log-function*
              (lambda (&rest rest)
                (declare (ignore rest))
                (setf result t)))
        (macrolet ((log-is-output (log-level expected)
                     `(progn (setf result nil)
                             (console-log :kind ,log-level "")
                             (ok (eq result ,expected)))))
          (set-console-log-level :error)
          (log-is-output :error t)
          (log-is-output :warning nil)
          (log-is-output :debug nil)
          (set-console-log-level :warning)
          (log-is-output :error t)
          (log-is-output :warning t)
          (log-is-output :debug nil)
          (set-console-log-level :debug)
          (log-is-output :error t)
          (log-is-output :warning t)
          (log-is-output :debug t)))))
  (testing "Error if an unknown log level is passed"
    (ok (signals (macroexpand-1 '(console-log :kind :unknown "test"))
                 'simple-error))
    (ok (signals (set-console-log-level :unknown)
                 'simple-error))))
