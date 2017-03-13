#|
  This file is a part of cl-web-2d-game project.
  Copyright (c) 2016 eshamster
|#

(in-package :cl-user)
(defpackage cl-web-2d-game-test-asd
  (:use :cl :asdf))
(in-package :cl-web-2d-game-test-asd)

(defsystem cl-web-2d-game-test
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:cl-web-2d-game
               :ps-experiment-test
               :cl-ppcre
               :prove
               :alexandria)
  :components ((:module "t"
                :serial t
                :components
                ((:file "test-utils")
                 (:test-file "utils")
                 (:test-file "basic-components")
                 (:test-file "calc")
                 (:test-file "collision"))))
  :description "Test system for cl-web-2d-game"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
