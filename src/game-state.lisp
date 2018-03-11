(defpackage cl-web-2d-game.game-state
  (:use :cl
        :ps-experiment)
  (:export :process-game-state
           :init-game-state

           :game-state
           :start-process
           :game-state-start-process
           :game-state-process
           :process
           :game-state-end-process
           :end-process)
  (:import-from :cl-ps-ecs
                :process))
(in-package :cl-web-2d-game.game-state)

(defstruct.ps+ game-state
  (start-process (lambda (_this) (declare (ignore _this)) t))
  (process (lambda (_this) (declare (ignore _this)) nil))
  (end-process (lambda (_this) (declare (ignore _this)) t)))

(defun.ps+ make-empty-game-state ()
  (make-game-state))

(defvar.ps+ *current-game-state* (make-empty-game-state))
(defvar.ps+ *next-game-state* nil)
(defvar.ps+ *current-sub-game-state* :start) ; :start, :run, :end

(defun.ps+ process-game-state ()
  (ecase *current-sub-game-state*
    (:start (when (funcall (game-state-start-process *current-game-state*)
                           *current-game-state*)
              (setf *current-sub-game-state* :run)))
    (:run (let ((result (funcall (game-state-process *current-game-state*)
                                 *current-game-state*)))
            (when result
              (setf *current-sub-game-state* :end)
              (setf *next-game-state* result))))
    (:end (when (funcall (game-state-end-process *current-game-state*)
                         *current-game-state*)
            (assert *next-game-state*)
            (setf *current-game-state* *next-game-state*)
            (setf *next-game-state* nil)
            (setf *current-sub-game-state* :start)))))

(defun.ps+ init-game-state (state)
  (check-type state game-state)
  (setf *current-game-state* state
        *current-sub-game-state* :start))
