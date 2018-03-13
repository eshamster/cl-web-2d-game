(in-package :cl-user)
(defpackage {{name}}-test
  (:use :cl
        :{{name}}
        :prove))
(in-package :{{name}}-test)

(plan 1)

(defvar *port* 21464)

;; Only test connection
(unwind-protect
     (progn
       ({{name}}:start :port *port*)
       (handler-case
           (ok (dex:get (format nil "http://localhost:~D" *port*)))
         (error (e)
           (fail (format nil "~A" e)))))
  ({{name}}:stop))

(finalize)
