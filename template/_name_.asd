#|
  This file is a part of {{name}} project.
  Copyright (c) 2018 {{author}} ({{email}})
|#

#|
  {{description}}

  Author: {{author}} ({{email}})
|#

(in-package :cl-user)
(defpackage {{name}}-asd
  (:use :cl :asdf))
(in-package :{{name}}-asd)

(defsystem {{name}}
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "{{author}}"
  :license "{{license}}"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack
               :{{name}}/src/{{name}})
  :description "{{description}}"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op {{name}}-test))))
