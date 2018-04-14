(in-package :cl-user)
(defpackage cl-web-2d-game/utils/debug/performance
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:import-from :alexandria
                :with-gensyms)
  (:export :with-trace
           :with-performance
           :dump-performance-counter))
(in-package :cl-web-2d-game/utils/debug/performance)

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

(defstruct.ps+ performance-timer-manager
    tree current-node (target-fps 60))

(defstruct.ps+ performance-timer-element
    (name "") (results (init-ring-buffer 30)) (count 0) (color 0))

(defstruct.ps+ performance-timer-node element (children '()))

;; utilities

(defstruct.ps+ ring-buffer array (count 0) (next 0))

(defun.ps+ init-ring-buffer (size)
  (make-ring-buffer :array (make-array size)))

(defun.ps+ push-to-ring-buffer (value buffer)
  (with-slots (array next count) buffer
    (setf (aref array next) value)
    (incf count)
    (setf next (if (< (1+ next) (length array))
                   (1+ next)
                   0))))

(defun.ps+ ring-buffer-average (buffer)
  (with-slots (array count) buffer
    (if (> count 0)
        (let ((valid-length (min count (length array))))
          (/ (loop for i from 0 below valid-length
                sum (aref array i))
             valid-length))
        0)))

;; variables

(defvar.ps+ *performance-timer* (make-performance-timer-manager))

;; functions to measure

(defun.ps+ pick-performance-timer-element
    (name color &optional (manager *performance-timer*))
  (check-type manager performance-timer-manager)
  (with-slots (tree current-node) manager
    (let ((found-node
           (If (not (null current-node))
               (find-if (lambda (node)
                          (string= (performance-timer-element-name
                                    (performance-timer-node-element node))
                                   name))
                        (performance-timer-node-children current-node))
               tree)))
      (if (not (null found-node))
          (progn (setf current-node found-node)
                 (performance-timer-node-element found-node))
          (let* ((new-elem (make-performance-timer-element
                            :name name :color color))
                 (new-node (make-performance-timer-node
                            :element new-elem)))
            (if (null tree)
                (setf tree new-node)
                (push new-node
                      (performance-timer-node-children current-node)))
            (setf current-node new-node)
            new-elem)))))

(defmacro.ps with-performance ((name &key (color 0)) &body body)
  (with-gensyms (prev-node before element)
    `(let ((,prev-node (performance-timer-manager-current-node
                       *performance-timer*)))
       (unwind-protect
            (let ((,before (performance.now))
                  (,element (pick-performance-timer-element ,name ,color)))
              ,@body
              (push-to-ring-buffer (- (performance.now) ,before)
                                   (performance-timer-element-results ,element)))
         (setf (performance-timer-manager-current-node
                *performance-timer*)
               ,prev-node)))))

(defmacro with-performance ((name &key color) &body body)
  ;; dummy to compile
  (declare (ignore name color))
  `(progn ,@body))


;; functions to draw

(defun.ps dump-performance-counter (&key (timer *performance-timer*))
  (labels ((format-number (num upper-digit lower-digit)
             (let ((temp (num.to-fixed lower-digit))
                   (min-length (+ upper-digit lower-digit 1)))
               (while (< (length temp) min-length)
                 (setf temp (+ "0" temp)))
               temp))
           (rec (result node)
             (let* ((element (performance-timer-node-element node))
                    (children (performance-timer-node-children node))
                    (time-ms (ring-buffer-average
                              (performance-timer-element-results element))))
               (setf result (+ result "("
                               (performance-timer-element-name element) ":"
                               (format-number time-ms 2 2)))
               (when children.length
                 (setf result (+ result " "))
                 (dolist (child children)
                   (setf result (rec result child))))
               (setf result (+ result ")"))
               result)))
    (rec "" (performance-timer-manager-tree timer))))
