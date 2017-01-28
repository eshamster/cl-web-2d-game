(in-package :cl-user)
(defpackage cl-web-2d-game-test.test-utils
  (:use :cl
        :prove
        :cl-web-2d-game.basic-components)
  (:export :use-packages-for-test
           :within
           :is-point
           :is-vector
           :*angle-error*
           :*length-error*)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game-test.test-utils)

;; --- not clasified --- ;;

(defun.ps+ within-p (got expected tolerance)
  (< (- expected tolerance)
     got
     (+ expected tolerance)))

(defmacro.ps+ within (got expected tolerance)
  `(progn
     (is ,got ,expected :test (lambda (got expected) (within-p got expected ,tolerance)))))

(defvar.ps+ *angle-error* (/ PI 10000))
(defvar.ps+ *length-error* (/ 1 10000))

(defmacro.ps+ is-vector (target x y)
  (with-gensyms (g-target)
    `(let ((,g-target ,target))
       (within (vector-2d-x ,g-target) ,x *length-error*)
       (within (vector-2d-y ,g-target) ,y *length-error*))))

(defmacro.ps+ is-point (target x y angle)
  (with-gensyms (g-target)
    `(let ((,g-target ,target))
       (within (point-2d-x ,g-target) ,x *length-error*)
       (within (point-2d-y ,g-target) ,y *length-error*)
       (within (point-2d-angle ,g-target) ,angle *angle-error*))))

;; --- use-packages-for-test --- ;;

(defvar *package-prefix* (string-upcase "cl-web-2d-game."))

(defun target-package-p (package)
  (ppcre:scan (format nil "^~A" *package-prefix*)
              (package-name package)))

(defun create-package-name (package-suffix &optional (package-prefix *package-prefix*))
  (concatenate 'string package-prefix (symbol-name package-suffix)))

(defun gather-used-packages (package-suffix &optional (package-prefix *package-prefix*))
  (labels ((rec (parent-name result)
             (dolist (pack (package-use-list parent-name))
               (let ((pack-name (package-name pack)))
                 (when (and (target-package-p pack)
                            (not (find pack-name result :test #'string=)))
                   (push pack-name result)
                   (rec pack-name result))))
             result))
    (let ((name (create-package-name package-suffix package-prefix)))
      (rec name (list name)))))

(defun use-packages-for-test (package-suffix)
  "Use required packages for test. For example, if you want to test
'cl-web-2d-game.calc', use this as (use-package-for-test :calc).
Note: Using the 'cl-web-2d-game' package is ideal. But it is impossible now,
because cl-js:run-js can't run if use it (the reason is not revealed)."
  (dolist (pack (gather-used-packages package-suffix))
    (use-package pack)))
