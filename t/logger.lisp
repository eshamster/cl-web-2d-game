(in-package :cl-user)
(defpackage cl-web-2d-game-test.calc
  (:use :cl
        :prove
        :cl-ps-ecs
        :cl-web-2d-game-test.test-utils)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both
                :prove-in-both
                :is-list.ps+))
(in-package :cl-web-2d-game-test.calc)

;; --- prepare --- ;;

(use-packages-for-test :logger)

(import 'cl-web-2d-game.logger::*current-console-log-level*)

(defmacro.ps+ with-logger-env (() &body body)
  (with-gensyms (pre-log-function pre-log-level)
    `(let ((,pre-log-function *console-log-function*)
           (,pre-log-level *current-console-log-level*))
       (unwind-protect
            (progn ,@body)
         (progn (setf *console-log-function* ,pre-log-function)
                (setf *current-console-log-level* ,pre-log-level))))))

;; --- test --- ;;

(plan 1)

(subtest "Test console logger"
  (subtest "Decide if log is output by log level"
    (with-prove-in-both ()
      (with-logger-env ()
        (let ((result nil))
          (setf *console-log-function*
                (lambda (&rest rest)
                  (declare (ignore rest))
                  (setf result t)))
          (macrolet ((log-is-output (log-level expected)
                       `(progn (setf result nil)
                               (console-log :kind ,log-level "")
                               (is result ,expected))))
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
            (log-is-output :debug t))))))
  (subtest "Error if an unknown log level is passed"
    (is-error (macroexpand-1 '(console-log :kind :unknown "test"))
              'simple-error)
    (with-prove-in-both ()
      (is-error (set-console-log-level :unknown)
                'simple-error))))

(finalize)
