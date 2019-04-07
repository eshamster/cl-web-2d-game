(defpackage cl-web-2d-game/utils/stage-generator
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :def-stage-element-interpreter.ps+
           :generate-stage
           :process-stage
           :stage)
  (:import-from :alexandria
                :with-gensyms
                :symbolicate))
(in-package :cl-web-2d-game/utils/stage-generator)

;; Note: This is only alpha quality.

;; --- data structure --- ;;

(defstruct.ps+ element (time 0) (func (lambda ())))

(defstruct.ps+ stage (current-time 0) (element-list (list)))

;; --- export --- ;;

(defmacro.ps+ def-stage-element-interpreter.ps+ (name lambda-key-form &body body)
  (with-gensyms (stage args)
    `(def-top-level-form.ps+ ,(symbolicate '_def-stage-element-interpreter_ name)
       (register-stage-element-interpreter
        ,name (lambda (,stage ,args)
                (apply (lambda (&key time ,@lambda-key-form)
                       (add-element-to-stage ,stage time
                                             (lambda () ,@body)))
                      ,args))))))

(defmacro.ps+ generate-stage (&body defs)
  (with-gensyms (result-stage)
    `(let ((,result-stage (make-stage)))
       ,@(generate-stage% result-stage defs)
       ,result-stage)))

(defun.ps+ process-stage (stage)
  (with-slots (current-time element-list) stage
    (labels ((delete-expired ()
               (when (and (> (length element-list) 0)
                          (> current-time
                             (element-time (car element-list))))
                 (setf element-list (cdr element-list))
                 (delete-expired))))
      (delete-expired))
    (labels ((process-current ()
               (when (and (> (length element-list) 0)
                          (= current-time
                             (element-time (car element-list))))
                 (funcall (element-func (car element-list)))
                 (setf element-list (cdr element-list))
                 (process-current))))
      (process-current))
    (incf current-time)))

;; --- internal --- ;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun.ps+ add-element-to-stage (stage time func)
    (with-slots (element-list) stage
      (push (make-element :time time :func func)
            element-list)
      (setf element-list
            (sort element-list
                  (lambda (a b)
                    (< (element-time a) (element-time b)))))))

  (defvar.ps+ *stage-element-interpreter-table* (make-hash-table))

  (defun.ps+ register-stage-element-interpreter (name func)
    (setf (gethash name *stage-element-interpreter-table*)
          func))

  (defun.ps+ interpret-element (stage kind &rest def)
    (let ((func (gethash kind *stage-element-interpreter-table*)))
      (funcall func stage def)))

  (defun.ps+ generate-stage% (result-stage def-list)
    (mapcar (lambda (def)
              (if (keywordp (car def))
                  `(interpret-element ,result-stage ,@def)
                  `(macrolet ((stage (&body body)
                                `(progn ,@(generate-stage% ',result-stage body))))
                     ,def)))
            def-list)))

;; --- definition --- ;;

(def-stage-element-interpreter.ps+ :general (func)
  (funcall func))
