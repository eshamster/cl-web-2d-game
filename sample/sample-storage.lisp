(in-package :cl-user)
(defpackage :cl-web-2d-game-sample.sample-storage
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :cl-web-2d-game-sample.common
                :use-this-package-as-sample))
(in-package :cl-web-2d-game-sample.sample-storage)

(use-this-package-as-sample)

;; --- Parenscript program --- ;;

(defvar.ps+ *storage-number* 1)

(defun.ps+ init-gui-panel ()
  (add-panel-number "storage number" *storage-number*
                    :min 1 :max 5 :step 1
                    :on-change (lambda (value)
                                 (setf *storage-number* value)))
  (let ((folder (add-panel-folder "store & read")))
    (add-panel-button "set random value"
                      :folder folder
                      :on-change (lambda ()
                                   (store-kvs *storage-number* (random))))
    (add-panel-button "read storage"
                      :folder folder
                      :on-change (lambda ()
                                   (add-to-event-log (read-kvs *storage-number*)))))
  (add-panel-button "clear storage"
                    :on-change (lambda ()
                                 (clear-kvs-all)
                                 (add-to-event-log "Clear ALL"))))

(defun.ps+ init-func (scene)
  (set-kvs-prefix "sample-storage-")
  (init-gui)
  (init-input)
  (init-gui-panel)
  (init-default-systems :scene scene))

(defun.ps+ update-func ())
