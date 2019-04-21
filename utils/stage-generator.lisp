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

(defstruct.ps+ interpreter func include-list)

(defstruct.ps+ stage (current-time 0) (element-list (list)))

;; --- export --- ;;

(defmacro.ps+ def-stage-element-interpreter.ps+
    (name-and-options lambda-key-form &body body)
  (multiple-value-bind (name include)
      (parse-name-and-options name-and-options)
    (with-gensyms (stage args immediate-p)
      `(def-top-level-form.ps+ ,(symbolicate '_def-stage-element-interpreter_ name)
         (register-stage-element-interpreter
          ,name (lambda (,stage ,args ,immediate-p)
                  (apply (lambda ,(generate-lambda-list
                                   lambda-key-form include)
                           (if ,immediate-p
                               (progn ,@body)
                               (add-element-to-stage ,stage time
                                                     (lambda () ,@body))))
                         ,args))
          ',include)))))

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
  (defun parse-name-and-options (name-and-options)
    "Return (values name include-list)"
    (if (listp name-and-options)
        (let* ((name (car name-and-options))
               (options (cdr name-and-options))
               (include-list (cdr (assoc :include options))))
          (values name include-list))
        name-and-options))

  (defun generate-lambda-list (lambda-key-form include-keyword-list)
    `(&key time ,@lambda-key-form
           ,@(mapcar #'symbolicate include-keyword-list))))

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

  (defun.ps+ get-stage-element-interpreter-table ()
    *stage-element-interpreter-table*)

  (defun.ps+ register-stage-element-interpreter (name func include-list)
    (setf (gethash name (get-stage-element-interpreter-table))
          (make-interpreter :func func
                            :include-list include-list)))

  (defun.ps+ interpret-element%% (stage kind args immediate-p)
    (let ((interpreter (gethash kind (get-stage-element-interpreter-table))))
      (funcall (interpreter-func interpreter) stage args immediate-p)))

  (defun interpret-element% (stage kind def-args &optional (immediate-p nil))
    (flet ((include-args-p (arg)
             (and (listp arg) (keywordp (car arg))))
           (extract-if (predicate sequence)
             (remove-if (lambda (elem) (not (funcall predicate elem)))
                        sequence)))
      (let* ((normal-args (remove-if #'include-args-p def-args))
             (include-args (extract-if #'include-args-p def-args))
             (interpreted-include-args
              (mapcan (lambda (args)
                        (let ((key (car args)))
                          (list key (interpret-element% stage key (cdr args) t))))
                      include-args)))
        `(interpret-element%% ,stage ,kind
                              (append (list ,@normal-args)
                                      (list ,@interpreted-include-args))
                              ,immediate-p))))

  (defun.ps+ generate-stage% (result-stage def-list)
    (mapcar (lambda (def)
              (if (keywordp (car def))
                  (interpret-element% result-stage (car def) (cdr def))
                  `(macrolet ((stage (&body body)
                                `(progn ,@(generate-stage% ',result-stage body))))
                     ,def)))
            def-list)))

;; --- definition --- ;;

(def-stage-element-interpreter.ps+ :general (func)
  (funcall func))
