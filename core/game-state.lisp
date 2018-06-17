(defpackage cl-web-2d-game/core/game-state
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
           :end-process

           :make-state
           :def-game-state)
  (:import-from :cl-ps-ecs
                :process))
(in-package :cl-web-2d-game/core/game-state)

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

;; --- experimental definition macro --- ;

(defvar.ps+ *state-maker-table* (make-hash-table))

(defun.ps+ make-state (kind &rest keys)
  "Make game state instance.
Please see also the document of the def-game-state for detail."
  (apply (gethash kind *state-maker-table*) keys))

(defun.ps+ register-state-maker (kind func)
  (setf (gethash kind *state-maker-table*) func))

(defmacro def-game-state (name (&rest params) &key start-process process end-process)
  "Define game state. This is mainly a wrapper of definition of a struct inheriting game-state.
You can create an instance by (make-state :name).
The \"params\" is same to the slot definition of defstruct.
Ex. (def-game-state name (param1 (param2 100))) -> (make-state :name :param1 999)
And you can access to the parameter by, for example, (slot-value <instance> 'param1)."
  `(progn
     (defstruct.ps+
         (,(intern (format nil "GAME-~A-STATE" name))
           (:include game-state
                     ,@(append (when start-process
                                 `((start-process ,start-process)))
                               (when process
                                 `((process ,process)))
                               (when end-process
                                 `((end-process ,end-process))))))
         ,@params)
     (def-top-level-form.ps+ ,(intern (format nil "GAME-STATE-REGISTER-~A" name))
       (register-state-maker
        ,(intern (symbol-name name) (find-package "KEYWORD"))
        (function ,(intern (format nil "MAKE-GAME-~A-STATE" name)))))))
