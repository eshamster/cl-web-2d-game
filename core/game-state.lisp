(defpackage cl-web-2d-game/core/game-state
  (:use :cl
        :ps-experiment)
  (:export :process-game-state
           :init-game-state
           :interrupt-game-state

           :game-state
           :start-process
           :game-state-start-process
           :game-state-process
           :process
           :game-state-end-process
           :end-process

           :game-state-manager
           :game-state-manager-current-state
           :game-state-manager-next-state
           :init-game-state-manager

           :make-state
           :def-game-state

           :state-lambda)
  (:import-from :cl-ps-ecs
                :process)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game/core/game-state)

(defstruct.ps+ game-state
  (start-process (lambda (_this) (declare (ignore _this)) t))
  (process (lambda (_this) (declare (ignore _this)) nil))
  (end-process (lambda (_this) (declare (ignore _this)) t)))

(defstruct.ps+ game-state-manager
  (current-state (make-empty-game-state))
  next-state
  (sub-state :before-start) ; :before-start, :start, :run, :end
  )

(defun.ps+ init-game-state-manager (initial-state)
  (check-type initial-state game-state)
  (make-game-state-manager :current-state initial-state))

(defun.ps+ make-empty-game-state ()
  (make-game-state))

(defvar.ps+ *global-game-state-manager* (make-game-state-manager))

(defun.ps+ interrupt-game-state (next-state &optional (manager *global-game-state-manager*))
  (check-type next-state game-state)
  (setf (game-state-manager-next-state manager)
        next-state))

(defun.ps+ process-game-state (&optional (manager *global-game-state-manager*))
  (with-slots (current-state next-state sub-state) manager
    (when (and (eq sub-state :before-start)
               next-state)
      ;; interrupted case
      (setf current-state next-state)
      (setf next-state nil))
    (ecase sub-state
      ((:before-start :start)
       (when (eq sub-state :before-start)
         (setf sub-state :start))
       (when (funcall (game-state-start-process current-state)
                      current-state)
         (setf sub-state :run)))
      (:run (let ((result (funcall (game-state-process current-state)
                                   current-state)))
              (cond
                (next-state ; interrupted case
                 (setf sub-state :end))
                (result
                 (check-type result game-state)
                 (setf sub-state :end)
                 (setf next-state result)))))
      (:end (when (funcall (game-state-end-process current-state)
                           current-state)
              (assert next-state)
              (setf current-state next-state)
              (setf next-state nil)
              (setf sub-state :before-start))))))

(defun.ps+ init-game-state (state &optional (manager *global-game-state-manager*))
  (check-type state game-state)
  (with-slots (current-state next-state sub-state) manager
    (setf current-state state
          next-state nil
          sub-state :before-start)))

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
And you can access to the parameter by, for example, (slot-value <instance> 'param1),
or please see the document of state-lambda for the same purpose."
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

;; --- --- ;;

(defmacro.ps+ state-lambda (slot-entries &body body)
  "state-lambda returns a lambda form that can be used for game-state definition.
It provides bindings by with-slots style slot-entiries."
  (with-gensyms (state)
    `(lambda (,state)
       (declare (ignorable ,state))
       ,(if slot-entries
            `(with-slots ,slot-entries ,state
               ,@body)
            `(progn ,@body)))))
