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
                         (:a :href "https://github.com/eshamster/cl-web-2d-game" "Cl-Web-2d-Game")
                         " / sample")
                        (:ul
                         (dolist (name '("simple" "animation"))
                           (markup (:li (:a :href (format nil "/sample-~A" name) name)))))))))))

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
                    (dolist (js-src (make-src-list-for-script-tag *js-relative-dir*))
                      (markup (:script :src js-src nil))))
                   (:body
                    (:div :id "stats-output")
                    (:div :id "renderer" nil)
                    (:div :id "monitor" "(for Monitoring Log)")
                    (:div (:pre :id "eventlog" "(for Event Log)"))
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
