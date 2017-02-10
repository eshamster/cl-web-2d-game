(in-package :cl-user)
(defpackage cl-web-2d-game-test.utils
  (:use :cl
        :prove
        :cl-ps-ecs
        :cl-web-2d-game-test.test-utils)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both))
(in-package :cl-web-2d-game-test.utils)

;; --- prepare --- ;;

(use-packages-for-test :utils)

;; --- test --- ;;

(plan 1)

(subtest "Test layered hash"
  (with-prove-in-both ()
    (let* ((x 0)
           (a-hash (convert-to-layered-hash
                    (:position (:x 12 :y (+ 10 20))
                     :size (:width (* 2 3) :height 100)
                     :with-variable (+ x 1)))))
      (is (get-layered-hash a-hash :position :x) 12)
      (is (get-layered-hash a-hash :position :y) 30)
      (is (get-layered-hash a-hash :size :width) 6)
      (is (get-layered-hash a-hash :size :height) 100))))

(finalize)
