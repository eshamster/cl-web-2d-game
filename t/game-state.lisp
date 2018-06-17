(defpackage cl-web-2d-game/t/game-state
  (:use :cl
        :rove
        :cl-ps-ecs
        :ps-experiment/t/test-utils
        :cl-web-2d-game/core/game-state
        :cl-web-2d-game/t/test-utils)
  (:import-from :ps-experiment
                :defvar.ps+
                :defun.ps+
                :defmacro.ps+
                :defstruct.ps+) 
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game/t/game-state)

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

(deftest.ps+ main
  (init-game-state (make-test-state1))
  (process-game-state)
  (ok (string= *buffer* "state1 start-process 0"))
  (process-game-state)
  (ok (string= *buffer* "state1 start-process 1"))
  (process-game-state)
  (ok (string= *buffer* "state1 process 0"))
  (process-game-state)
  (ok (string= *buffer* "state1 process 1"))
  (process-game-state)
  (ok (string= *buffer* "state1 end-process 0"))
  (process-game-state)
  (ok (string= *buffer* "state1 end-process 1"))
  (process-game-state)
  (ok (string= *buffer* "state2 start-process"))
  ;; reset
  (init-game-state (make-test-state1))
  (process-game-state)
  (ok (string= *buffer* "state1 start-process 0")))


;; --------------------------- ;;
;; --- test def-game-state --- ;;
;; --------------------------- ;;

(def-game-state test-def-state1 (test-param1 (test-param2 100))
  :start-process
  (lambda (_this)
    (incf (slot-value _this 'test-param1))
    (write-buffer-with-clear "start test-def-state1")
    t)
  :process
  (lambda (_this)
    (write-buffer-with-clear
     (+ (slot-value _this 'test-param1) (slot-value _this 'test-param2)))
    (make-state :test-def-state2))
  :end-process
  (lambda (_this)
    (write-buffer-with-clear "end test-def-state1")
    t))

(def-game-state test-def-state2 ()
  :start-process
  (lambda (_this)
    (declare (ignore (_this)))
    (write-buffer-with-clear "start test-def-state2")))

(deftest.ps+ for-def-game-state
  (init-game-state (make-state :test-def-state1
                               :test-param1 1000))
  (process-game-state)
  (ok (string= *buffer* "start test-def-state1"))
  (process-game-state)
  (ok (= *buffer* 1101))
  (process-game-state)
  (ok (string= *buffer* "end test-def-state1"))
  (process-game-state)
  (ok (string= *buffer* "start test-def-state2")))
