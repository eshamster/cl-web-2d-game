#|
  This file is a part of {{name}} project.
  Copyright (c) 2017 {{author}} ({{email}})
|#

(in-package :cl-user)
(defpackage {{name}}-test-asd
  (:use :cl :asdf))
(in-package :{{name}}-test-asd)

(defsystem {{name}}-test
  :author "{{author}}"
  :license "{{license}}"
  :depends-on (:{{author}}
               :prove
               :dexador)
  :components ((:module "t"
                :components
                ((:test-file "{{name}}"))))
  :description "Test system for {{name}}"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
