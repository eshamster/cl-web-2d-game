(in-package :cl-user)
(defpackage cl-web-2d-game/utils/utils
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :convert-to-layered-hash
           :get-layered-hash
           :ensure-js-files
           :make-src-list-for-script-tag
           :def-obsoleted-alias.ps+)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game/utils/utils)

(enable-ps-experiment-syntax)

;; --- constant value manager --- ;;

(defun map-pair (function list)
    "Ex. (map-pair (lambda (a b) (+ a b)) '(1 2 3 4)) => '(3 7)"
    (labels ((rec (rest result)
               (if rest
                   (rec (cddr rest)
                        (cons (funcall function (car rest) (cadr rest)) result))
                   result)))
      (nreverse (rec list nil))))

#|
Example:
(defvar *hash*
  (convert-to-layered-hash
   (:position (:x 12 :y (+ 10 20))
    :size (:width (* 2 3) :height 100)
    :some-list (list 1 2 3))))

(get-layered-hash *hash* :position :x)  => 12
(get-layered-hash *hash* :position :y)  => 30
(get-layered-hash *hash* :size :width)  => 6
(get-layered-hash *hash* :size :height) => 100
(get-layered-hash *hash* :some-list) => (1 2 3)
|#
(defmacro.ps+ convert-to-layered-hash (list)
  (labels ((is-pair (element)
             (and (listp element)
                  (string= (package-name (symbol-package (car element)))
                           "KEYWORD")))
           (convert-value (value)
             (if (listp value)
                 `(lambda () ,value)
                 value))
           (make-hash-insertion (rest)
             `(let ((result (make-hash-table)))
                ,@(map-pair (lambda (key value)
                              `(setf (gethash ,key result)
                                     ,(if (is-pair value)
                                          (make-hash-insertion value)
                                          (convert-value value))))
                            rest)
                result)))
    (make-hash-insertion list)))

(defmacro.ps+ get-layered-hash (hash &rest keys)
  (labels ((rec (rest-keys result)
             (if rest-keys
                 (rec (cdr rest-keys)
                      `(gethash ,(car rest-keys) ,result))
                 result)))
    `(let ((value ,(rec keys hash)))
       (if (functionp value)
           (funcall value)
           value))))

;; --- CL tools for building application --- ;

(defparameter *cdns*
  '("https://cdnjs.cloudflare.com/ajax/libs/three.js/86/three.js"
    "https://cdnjs.cloudflare.com/ajax/libs/dat-gui/0.6.3/dat.gui.js"
    "https://cdnjs.cloudflare.com/ajax/libs/stats.js/r16/Stats.js"))

(defparameter *js-pairs*
  '(("threex.keyboardstate.js" . "https://raw.githubusercontent.com/jeromeetienne/threex.keyboardstate/51fd77fdd87eeed064db643693d393cf21afa45d/threex.keyboardstate.js")
    ("wtf-trace.js" . "https://raw.githubusercontent.com/google/tracing-framework/b08cb6e3bc7287fad4a70bc2fceda34d7077fc60/shims/wtf-trace.js")
    ("helvetiker_regular.typeface.json" . "https://raw.githubusercontent.com/mrdoob/three.js/e6c0d10835a75952da6bd430c5269ce38740d102/examples/fonts/helvetiker_regular.typeface.json")))

(defun ensure-js-files (dir)
  (ensure-directories-exist dir)
  (dolist (pair *js-pairs*)
    (let ((path (merge-pathnames (car pair) dir))
          (url (cdr pair)))
      (unless (probe-file path)
        (with-open-file (file path
                              :direction :output
                              :if-exists :error
                              :if-does-not-exist :create)
          (format *error-output* "Download: ~A" (car pair))
          (princ (dex:get url) file))))))

(defun make-src-list-for-script-tag (relative-path)
  (append *cdns*
          (remove-if (lambda (path)
                       (not (string= (pathname-type path) "js")))
                     (mapcar (lambda (pair)
                               (merge-pathnames (car pair) relative-path))
                             *js-pairs*))))

;; --- def-obsoleted-fun.ps+ --- ;;

;; TODO: Move the definition to more proper place.

(defmacro def-obsoleted-alias.ps+ (obsoleted-name alter-fn)
  (with-gensyms (rest)
    `(defmacro.ps+ ,obsoleted-name (&rest ,rest)
       (warn ,(format nil "\"~A\" is obsoleted. Please use \"~A\" instead."
                      obsoleted-name alter-fn))
       `(,',alter-fn ,@,rest))))
