(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.common
  (:use :cl)
  (:export :use-this-package-as-sample
           :make-js-main-file))
(in-package :cl-web-2d-game-sample.common)

(defvar *sample-package-table (make-hash-table))

(defmacro use-this-package-as-sample (&key
                                        (init-func (intern "INIT-FUNC" *package*))
                                        (update-func (intern "UPDATE-FUNC" *package*)))
  `(progn
     (defun ,(intern "OUTPUT-JS-CODE" *package*) (stream)
       (princ
        (pse:with-use-ps-pack (:this)
          (let ((width 800)
                (height 600))
            (cl-web-2d-game:start-2d-game
             :screen-width width
             :screen-height height
             :resize-to-screen-p t
             :camera (cl-web-2d-game:init-camera 0 0 width height)
             :rendered-dom (document.query-selector "#renderer")
             :stats-dom (document.query-selector "#stats-output")
             :monitoring-log-dom (document.query-selector "#monitor")
             :event-log-dom (document.query-selector "#eventlog")
             :init-function ,init-func
             :update-function ,update-func)))
        stream))))

(defun make-js-main-file (name path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (funcall (intern "OUTPUT-JS-CODE"
                     (find-package (format nil "CL-WEB-2D-GAME-SAMPLE.SAMPLE-~A"
                                           (string-upcase name))))
             out)))
