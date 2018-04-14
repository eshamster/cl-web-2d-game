(defpackage cl-web-2d-game/t/utils
  (:use :cl
        :rove
        :cl-ps-ecs
        :cl-web-2d-game/utils/utils
        :cl-web-2d-game/t/test-utils
        :ps-experiment/t/test-utils))
(in-package :cl-web-2d-game/t/utils)

;; --- test --- ;;

(deftest.ps+ for-layered-hash
  (let* ((x 0)
         (a-hash (convert-to-layered-hash
                  (:position (:x 12 :y (+ 10 20))
                             :size (:width (* 2 3) :height 100)
                             :with-variable (+ x 1)))))
    (ok (= (get-layered-hash a-hash :position :x) 12))
    (ok (= (get-layered-hash a-hash :position :y) 30))
    (ok (= (get-layered-hash a-hash :size :width) 6))
    (ok (= (get-layered-hash a-hash :size :height) 100))
    ;; check a value using variable
    (ok (= (get-layered-hash a-hash :with-variable) 1))
    (setf x 100)
    (ok (= (get-layered-hash a-hash :with-variable) 101))))
