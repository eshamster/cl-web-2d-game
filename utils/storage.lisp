(defpackage cl-web-2d-game/utils/storage
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :set-kvs-prefix
           :with-kvs-prefix
           :store-kvs
           :read-kvs
           :remove-kvs
           :clear-kvs-all)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game/utils/storage)

#|
Experimental key-value storage using Web Storage (localStorage)
|#

(enable-ps-experiment-syntax)

(defvar.ps+ *storage-prefix* "")

(defun.ps set-kvs-prefix (prefix)
  (setf *storage-prefix* prefix))

(defmacro.ps+ with-kvs-prefix ((key) &body body)
  (with-gensyms (org-prefix)
    `(let ((,org-prefix *storage-prefix*))
       (unwind-protect
            (progn (set-kvs-prefix ,key)
                   ,@body)
         (set-kvs-prefix ,org-prefix)))))

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
