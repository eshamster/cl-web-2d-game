(defpackage cl-web-2d-game/t/basic-components
  (:use :cl
        :rove
        :cl-ps-ecs
        :ps-experiment/t/test-utils
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/t/test-utils)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+))
(in-package :cl-web-2d-game/t/basic-components)

;; --- test --- ;;

(deftest.ps+ for-copy-funcitons
  (testing "vector"
    (testing "clone"
      (let* ((base (make-vector-2d :x 10 :y 20))
             (cloned (clone-vector-2d base)))
        (ok (is-vector cloned 10 20))
        (setf (vector-2d-x cloned) 99)
        (ok (is-vector cloned 99 20))
        (ok (is-vector base 10 20))))
    (testing "copy to"
      (let* ((base (make-vector-2d :x 10 :y 20))
             (copied (make-vector-2d)))
        (copy-vector-2d-to copied base)
        (ok (is-vector copied 10 20))
        (setf (vector-2d-x copied) 99)
        (ok (is-vector copied 99 20))
        (ok (is-vector base 10 20)))))
  (testing "point"
    (testing "clone"
      (let* ((base (make-point-2d :x 10 :y 20 :angle -1))
             (cloned (clone-point-2d base)))
        (ok (is-point cloned 10 20 -1))
        (setf (point-2d-x cloned) 99)
        (ok (is-point cloned 99 20 -1))
        (ok (is-point base 10 20 -1))))
    (testing "copy to"
      (let* ((base (make-point-2d :x 10 :y 20 :angle -1))
             (copied (make-point-2d)))
        (copy-point-2d-to copied base)
        (ok (is-point copied 10 20 -1))
        (setf (point-2d-x copied) 99)
        (ok (is-point copied 99 20 -1))
        (ok (is-point base 10 20 -1))))))

(deftest.ps+ for-params
  (testing "normals"
    (let ((entity (make-ecs-entity)))
      (add-ecs-component (init-entity-params
                          :test-param1 100
                          :test-param2 (lambda () (+ 10 20)))
                         entity)
      (ok (= (get-entity-param entity :test-param1) 100))
      (ok (= (funcall (get-entity-param entity :test-param2)) 30))
      (set-entity-param entity :test-param1 200)
      (ok (= (get-entity-param entity :test-param1) 200))
      (aset-entity-param entity :test-param1 (+ it 100))
      (ok (= (get-entity-param entity :test-param1) 300)))
    (testing "set to entity that has no params"
      (let ((entity (make-ecs-entity)))
        (ok (= (set-entity-param entity :test 100) 100))
        (ok (= (get-entity-param entity :test) 100))))
    (testing "set multiple params"
      (let ((entity (make-ecs-entity)))
        (set-entity-param entity
                          :test1 100
                          :test2 200)
        (ok (= (get-entity-param entity :test1) 100))
        (ok (= (get-entity-param entity :test2) 200))))
    (testing "setf get-entity-param"
      (let ((entity (make-ecs-entity)))
        (add-ecs-component (init-entity-params :test 100)
                           entity)
        (ok (= (get-entity-param entity :test) 100))
        (ok (= (setf (get-entity-param entity :test) 200) 200))
        (ok (= (get-entity-param entity :test) 200)))))
  (testing "errors"
    (ok (signals (init-entity-params :x 0 :without-value)
                 'simple-error))))
