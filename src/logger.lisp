(in-package :cl-user)
(defpackage cl-web-2d-game.logger
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
           :add-to-event-log))
(in-package :cl-web-2d-game.logger)

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
