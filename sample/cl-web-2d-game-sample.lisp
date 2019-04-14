(in-package :cl-user)
(defpackage :cl-web-2d-game-sample
  (:use :cl
        :cl-markup)
  (:export :start
           :stop)
  (:import-from :cl-web-2d-game
                :make-src-list-for-script-tag
                :ensure-js-files)
  (:import-from :cl-web-2d-game-sample.common
                :make-js-main-file))
(in-package :cl-web-2d-game-sample)

(defvar *sample-dir*
  (merge-pathnames "sample/"
                   (asdf:component-pathname
                    (asdf:find-system :cl-web-2d-game))))

(defvar *js-relative-dir* "js/")

(defvar *js-dir*
  (merge-pathnames *js-relative-dir* *sample-dir*))

;; --- route settings --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defmacro with-cl-markup (&body body)
  (let ((g-str (gensym)))
    `(with-output-to-string (,g-str)
       (let ((cl-markup:*output-stream* ,g-str))
         ,@body))))

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (with-cl-markup
          (html5 (:head
                  (:title "Cl-Web-2d-Game samples")
                  (:link :rel "stylesheet" :type "text/css" :href "css/style.css" nil))
                 (:body
                  (:div :id "panel"
                        (:div
                         :id "panel-content"
                         (:div
                          (:a :href "https://github.com/eshamster/cl-web-2d-game" "Cl-Web-2d-Game")
                          " / sample")
                         (:ul
                          :id "sample-list"
                          (dolist (name '("simple"
                                          ;; The followings are sorted by name.
                                          "animation"
                                          "basic-models"
                                          "collision"
                                          "input"
                                          "storage"
                                          "texture"
                                          "text"
                                          "ui"))
                            (markup (:li :class "sample-list-element"
                                         (:a :href (format nil "/sample-~A" name)
                                             :target "viewer"
                                             name)
                                         " ("
                                         (:a :href (format nil "https://github.com/eshamster/cl-web-2d-game/blob/master/sample/sample-~A.lisp" name)
                                             :target "_blank"
                                             "code")
                                         ")"))))))
                  (:iframe :id "viewer" :name "viewer" nil))))))

(setf (ningle:route *app* "/sample-*" :method :GET)
      (lambda (params)
        (let ((name (getf (car params) :splat)))
          ;; TODO: error response when the name is not exist
          (make-js-main-file name (merge-pathnames
                                   (format nil "sample-~A.js" name)
                                   *js-dir*))
          (with-cl-markup
            (html5 (:head
                    (:title (format nil "Sample: ~A" name))
                    (:link :rel "stylesheet" :type "text/css" :href "css/viewer.css" nil)
                    (dolist (js-src (make-src-list-for-script-tag *js-relative-dir*))
                      (markup (:script :src js-src nil))))
                   (:body
                    (:div :id "stats-output" nil)
                    (:div :id "renderer" nil)
                    (dolist (id '("monitor" "eventlog"))
                      (markup
                       (:div
                        :class "log-box"
                        (:label :class "open-close-label"
                                :id (format nil "open-close-label-~A" id)
                                :for (format nil "open-close-~A" id) nil)
                        (:input :class "open-close"
                                :id (format nil "open-close-~A" id)
                                :type "checkbox")
                        (:div :class "log-panel" :id id "(Log Area)"))))
                    (:script :src
                             (format nil "~Asample-~A.js" *js-relative-dir* name)
                             nil)))))))

(defvar *server* nil)

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun start (&key (port 5000))
  (stop)
  (ensure-js-files *js-dir*)
  (setf *server*
        (clack:clackup
         (lack:builder
          (:static :path (lambda (path)
                           (print path)
                           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)"
                                           path)
                               path
                               nil))
                   :root *sample-dir*)
          *app*) 
         :port port
         :use-thread nil)))
