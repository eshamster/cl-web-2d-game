(in-package :cl-user)
(defpackage cl-web-2d-game/utils/dom-manager
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :get-managed-dom
           :get-rendered-dom))
(in-package :cl-web-2d-game/utils/dom-manager)

(enable-ps-experiment-syntax)

(defvar.ps+ *dom-store* (make-hash-table))

(defmacro.ps+ get-managed-dom (key)
  `(gethash ,key *dom-store*))

(defmacro.ps+ get-rendered-dom ()
  `(get-managed-dom :render))
