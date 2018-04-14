(in-package :cl-user)
(defpackage cl-web-2d-game/graphics/font
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :load-font
           :get-font-promise))
(in-package :cl-web-2d-game/graphics/font)

(enable-ps-experiment-syntax)

(defvar.ps+ *font-name-to-promise-table* (make-hash-table))

(defun.ps make-font-name (name weight)
  (+ name "_" weight))

(defun.ps load-font (relative-path &key (name "helvetiker") (weight "regular"))
  "Load and register texture.
After that you can get it by get-font-promise with the name and the weight"
  (let ((loader (new (#j.THREE.FontLoader#)))
        (path (+ relative-path name "_" weight ".typeface.json")))
    (setf (gethash (make-font-name name weight) *font-name-to-promise-table*)
          (init-frame-promise
           (lambda (resolve)
             (loader.load path
                          (lambda (response)
                            (resolve response))))))))

(defun.ps+ get-font-promise (name weight)
  (let ((result (gethash (make-font-name name weight) *font-name-to-promise-table*)))
    (unless result
      (error (format nil "The font (name = ~A and weight = ~A) has not been loaded." name weight)))
    result))
