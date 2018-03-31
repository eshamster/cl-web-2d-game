(in-package :cl-user)
(defpackage cl-web-2d-game/utils/debug/logger
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :init-monitoring-log
           :clear-monitoring-log
           :add-to-monitoring-log
           
           :*max-event-log-count*
           :init-event-log-area
           :add-to-event-log

           :*console-log-function*
           :set-console-log-level
           :console-log))
(in-package :cl-web-2d-game/utils/debug/logger)

(enable-ps-experiment-syntax)

;; --- monitoring log --- ;;

;; The monitoring log is cleared in every frame.
;; This is intended to monitor data that is updated in every frame.

(defvar.ps *monitoring-log-area* nil)

(defun.ps init-monitoring-log (dom)
  (setf *monitoring-log-area* dom))

(defun.ps clear-monitoring-log ()
  (when *monitoring-log-area*
    (setf #j.*monitoring-log-area*.innerHTML# "Monitoring Log: ")))

(defun.ps add-to-monitoring-log (text)
  (when *monitoring-log-area*
    (incf #j.*monitoring-log-area*.innerHTML# (+ text "<br>"))))

;; --- event log --- ;;

;; The event log is as queue.
;; A new message is displayed the top of the log are.
;; Then, the bottom (= oldest) message is deleted if the queue is full.

(defvar.ps *event-log-area* nil)
(defvar.ps+ *event-log-text-list* '())
(defvar.ps+ *max-event-log-count* 5)

(defun.ps init-event-log-area (dom)
  (setf *event-log-area* dom))

(defun.ps add-to-event-log (text)
  (when *event-log-area*
    (push text *event-log-text-list*)
    (when (> (length *event-log-text-list*) *max-event-log-count*)
      (setf *event-log-text-list* (subseq *event-log-text-list*
                                          0 *max-event-log-count*)))
    (let ((log ""))
      ;; A newer log comes to the top.
      (dolist (one-line *event-log-text-list*)
        (setf log (+ log one-line "<br>")))
      (setf #j.*event-log-area*.innerHTML# log))))

;; --- console log --- ;;

(defvar *console-log-function* (lambda (log-kind log-level control-string &rest args)
                                 (eval `(format t (format nil "~D: ~D: ~D"
                                                          ,log-kind
                                                          ,log-level
                                                          ,control-string)
                                                ,@args))))

(defvar.ps *console-log-function* (lambda (log-kind log-level control-string &rest args)
                                    (console.log (+ log-kind ": "
                                                    log-level ": "
                                                    control-string
                                                    " (args = " args ")"))))

(defvar.ps+ +console-log-level-debug+ 10)
(defvar.ps+ +console-log-level-warning+ 20)
(defvar.ps+ +console-log-level-error+ 30)

;; TODO: Set log level for each log-kind

(defvar.ps+ *current-console-log-level* +console-log-level-error+)
(defun.ps+ get-console-log-level ()
  *current-console-log-level*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun.ps+ convert-to-console-log-level-symbol (log-level-keyward)
    (case log-level-keyward
      (:debug '+console-log-level-debug+)
      (:warning '+console-log-level-warning+)
      (:error '+console-log-level-error+)
      (t (error "The console level ~D is not recognized" log-level-keyward)))))

(defun.ps+ set-console-log-level (log-level)
  ;; Note: In Common Lisp, "symbol-value" is more appropriate than "eval".
  ;; However, in JavaScript, the former is not exist. So "eval" is selcted here.
  ;; In addition, because convert-to-console-log-level-symbol should return only
  ;; a known symbol, "eval" should not be so dangerous.
  (setf *current-console-log-level*
        (eval (convert-to-console-log-level-symbol log-level))))

;; This is defined as macro in order not to affect to main processes
(defmacro.ps+ console-log (log-kind log-level control-string &rest args)
  `(when (<= (get-console-log-level)
             ,(convert-to-console-log-level-symbol log-level))
     (funcall *console-log-function* ,log-kind ,log-level ,control-string ,@args)))
