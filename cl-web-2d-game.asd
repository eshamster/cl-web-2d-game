#|
  This file is a part of cl-web-2d-game project.
  Copyright (c) 2016 eshamster
|#

#|
  A library to create 2d game using Parenscript and three.js

  Author: eshamster
|#

(in-package :cl-user)
(defpackage cl-web-2d-game-asd
  (:use :cl :asdf))
(in-package :cl-web-2d-game-asd)

(defsystem cl-web-2d-game
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :dexador
               :cl-ps-ecs
               :cl-reexport)
  :components ((:module "src"
                :serial t
                :components
                ((:file "basic-components")
                 (:file "utils")
                 (:file "performance")
                 (:file "calc")
                 (:file "camera")
                 (:file "collision")
                 (:file "input")
                 (:file "2d-geometry")
                 (:file "draw-model-system")
                 (:file "gui")
                 (:file "logger")
                 (:file "initializer")
                 (:file "cl-web-2d-game"))))
  :description "A library to create 2d game using Parenscript and three.js"
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
  :in-order-to ((test-op (test-op cl-web-2d-game-test))))
