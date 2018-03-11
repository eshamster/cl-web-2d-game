(in-package :cl-user)
(defpackage cl-web-2d-game-test.game-state
  (:use :cl
        :prove
        :cl-ps-ecs
        :cl-web-2d-game-test.test-utils)
  (:import-from :ps-experiment
                :defvar.ps+
                :defun.ps+
                :defmacro.ps+
                :defstruct.ps+)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game-test.game-state)

;; --- prepare --- ;;

(use-packages-for-test :game-state)

;; - buffer - ;;

(defvar.ps+ *buffer* "")

(defun.ps+ write-buffer-with-clear (content)
  (setf *buffer* content))

;; - state - ;;

(defstruct.ps+ (test-state (:include game-state)) (count 0))

(defun.ps+ reset-count (_this)
  (setf (test-state-count _this) 0))

(defmacro.ps+ with-test-count (_this &body body)
  (with-gensyms (prev)
    `(progn (let ((,prev (test-state-count ,_this)))
              (incf (test-state-count ,_this))
              (case ,prev
                ,@body)))))

(defstruct.ps+
    (test-state2
     (:include test-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (write-buffer-with-clear "state2 start-process")
                  t)))))

(defstruct.ps+
    (test-state1
     (:include test-state
               (start-process
                (lambda (_this)
                  (with-test-count _this
                    (0 (write-buffer-with-clear "state1 start-process 0")
                       nil)
                    (1 (write-buffer-with-clear "state1 start-process 1")
                       (reset-count _this)
                       t))))
               (process
                (lambda (_this)
                  (with-test-count _this
                    (0 (write-buffer-with-clear "state1 process 0")
                       nil)
                    (1 (write-buffer-with-clear "state1 process 1")
                       (reset-count _this)
                       (make-test-state2)))))
               (end-process
                (lambda (_this)
                  (with-test-count _this
                    (0 (write-buffer-with-clear "state1 end-process 0")
                       nil)
                    (1 (write-buffer-with-clear "state1 end-process 1")
                       (reset-count _this)
                       t)))))))

;; --- test --- ;;

(plan 1)

(subtest "Main"
  (with-prove-in-both ()
    (init-game-state (make-test-state1))
    (process-game-state)
    (is *buffer* "state1 start-process 0")
    (process-game-state)
    (is *buffer* "state1 start-process 1")
    (process-game-state)
    (is *buffer* "state1 process 0")
    (process-game-state)
    (is *buffer* "state1 process 1")
    (process-game-state)
    (is *buffer* "state1 end-process 0")
    (process-game-state)
    (is *buffer* "state1 end-process 1")
    (process-game-state)
    (is *buffer* "state2 start-process")
    ;; reset
    (init-game-state (make-test-state1))
    (process-game-state)
    (is *buffer* "state1 start-process 0")))

(finalize)
