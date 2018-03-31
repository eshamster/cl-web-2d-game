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

(asdf:register-system-packages :ps-experiment '(:ps-experiment.common-macros))

(defsystem cl-web-2d-game
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :dexador
               :cl-ps-ecs
               :cl-web-2d-game/main) 
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
