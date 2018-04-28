(defpackage cl-web-2d-game/utils/storage
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :set-kvs-prefix
           :store-kvs
           :read-kvs
           :remove-kvs
           :clear-kvs-all))
(in-package :cl-web-2d-game/utils/storage)

#|
Experimental key-value storage using Web Storage (localStorage)
|#

(enable-ps-experiment-syntax)

(defvar.ps+ *storage-prefix* "")

(defun.ps set-kvs-prefix (prefix)
  (setf *storage-prefix* prefix))

(defun.ps add-prefix (key)
  (+ *storage-prefix* key))

(defun.ps store-kvs (key value)
  (local-storage.set-item (add-prefix key) value))

(defun.ps read-kvs (key)
  (local-storage.get-item (add-prefix key)))

(defun.ps remove-kvs (key)
  (local-storage.remove-item (add-prefix key)))

(defun.ps clear-kvs-all ()
  (local-storage.clear))

;; TODO: Add function to remove keys by prefix
