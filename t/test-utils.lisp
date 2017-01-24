(in-package :cl-user)
(defpackage cl-web-2d-game-test.test-utils
  (:use :cl
        :prove)
  (:export :use-packages-for-test))
(in-package :cl-web-2d-game-test.test-utils)

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
