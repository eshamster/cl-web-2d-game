(in-package :cl-user)
(defpackage cl-web-2d-game.performance
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:import-from :alexandria
                :with-gensyms)
  (:export :with-trace
           :with-performance
           :dump-performance-counter))
(in-package :cl-web-2d-game.performance)

(enable-ps-experiment-syntax)

;; --- performance tracer --- ;;

;; Note: this is depend on Web Tracing Framework (wtf-trace.js)

(defmacro.ps with-trace (title &body body)
  `(let ((scope (#j.WTF.trace.enterScope# ,title)))
     ,@body
     (#j.WTF.trace.leaveScope# scope ,title)))

(defmacro with-trace (title &body body)
  "(dummy)"
  (declare (ignore title))
  `(progn ,@body))

;; --- realtime performance tracer --- ;;

;; structures

(defstruct.ps+ performance-timer-manager (tree '()) (target-fps 60) (current-node nil))

(defstruct.ps+ performance-timer-element (name "") (result -1) (color 0))

(defstruct.ps+ performance-timer-node element (children '()))

;; variables

(defvar.ps+ *performance-timer* (make-performance-timer-manager))

;; functions to measure

(defun.ps+ add-performance-timer-element (element &optional (manager *performance-timer*))
  (check-type element performance-timer-element)
  (check-type manager performance-timer-manager)
  (let ((new-node (make-performance-timer-node
                   :element element)))
    (with-slots (tree current-node) manager
      ;; TODO: Measure total time from a previous frame
      (if (null current-node)
          (setf tree new-node)
          (progn (push new-node
                       (performance-timer-node-children current-node))))
      (setf current-node new-node))))

(defmacro.ps with-performance ((name &key (color 0)) &body body)
  (with-gensyms (prev-node before element)
    `(let ((,prev-node (performance-timer-manager-current-node
                       *performance-timer*)))
       (unwind-protect
            (let ((,before (performance.now))
                  (,element (make-performance-timer-element
                            :name ,name :color ,color)))
              (add-performance-timer-element ,element)
              ,@body
              (setf (performance-timer-element-result ,element)
                    (- (performance.now) ,before)))
         (setf (performance-timer-manager-current-node
                *performance-timer*)
               prev-node)))))

(defmacro with-performance ((name &key color) &body body)
  ;; dummy to compile
  (declare (ignore name color))
  `(progn ,@body))


;; functions to draw

(defun.ps dump-performance-counter (&key (timer *performance-timer*))
  (labels ((rec (result node)
             (when node.length
               (let* ((element (performance-timer-node-element node))
                      (children (peformance-timer-node-children node)))
                 (setf result (+ result "("
                                 (peformance-timer-element-name element) ":"
                                 (peformance-timer-element-result element)))
                 (when children.length
                   (setf result (+ result " "))
                   (dolist (child children)
                     (setf result (rec result child))))
                 (setf result (+ result ")"))))))
    (rec "" (performance-timer-manager-tree timer))))
