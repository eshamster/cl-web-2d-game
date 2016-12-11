(in-package :cl-user)
(defpackage cl-web-2d-game.utils
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :convert-to-layered-hash
           :get-layered-hash))
(in-package :cl-web-2d-game.utils)

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
