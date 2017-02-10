(in-package :cl-user)
(defpackage cl-web-2d-game-test.basic-components
  (:use :cl
        :prove
        :cl-ps-ecs
        :cl-web-2d-game-test.test-utils)
  (:import-from :ps-experiment
                :defmacro.ps+
                :defun.ps+
                :defvar.ps+)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both
                :prove-in-both))
(in-package :cl-web-2d-game-test.basic-components)

;; --- prepare --- ;;

(use-packages-for-test :basic-components)

;; --- test --- ;;

(plan 2)

(subtest "Test copy funcitons"
  (subtest "vector"
    (subtest "clone"
      (with-prove-in-both ()
        (let* ((base (make-vector-2d :x 10 :y 20))
               (cloned (clone-vector-2d base)))
          (is-vector cloned 10 20)
          (setf (vector-2d-x cloned) 99)
          (is-vector cloned 99 20)
          (is-vector base 10 20))))
    (subtest "copy to"
      (with-prove-in-both ()
        (let* ((base (make-vector-2d :x 10 :y 20))
               (copied (make-vector-2d)))
          (copy-vector-2d-to copied base)
          (is-vector copied 10 20)
          (setf (vector-2d-x copied) 99)
          (is-vector copied 99 20)
          (is-vector base 10 20)))))
  (subtest "point"
    (subtest "clone"
      (with-prove-in-both ()
        (let* ((base (make-point-2d :x 10 :y 20 :angle -1))
               (cloned (clone-point-2d base)))
          (is-point cloned 10 20 -1)
          (setf (point-2d-x cloned) 99)
          (is-point cloned 99 20 -1)
          (is-point base 10 20 -1))))
    (subtest "copy to"
      (with-prove-in-both ()
        (let* ((base (make-point-2d :x 10 :y 20 :angle -1))
               (copied (make-point-2d)))
          (copy-point-2d-to copied base)
          (is-point copied 10 20 -1)
          (setf (point-2d-x copied) 99)
          (is-point copied 99 20 -1)
          (is-point base 10 20 -1))))))

(subtest "Test params"
  (subtest "normals"
    (with-prove-in-both ()
      (let ((entity (make-ecs-entity)))
        (add-ecs-component (init-entity-params
                            :test-param1 100
                            :test-param2 (lambda () (+ 10 20)))
                           entity)
        (is (get-entity-param entity :test-param1) 100)
        (is (funcall (get-entity-param entity :test-param2)) 30)
        (set-entity-param entity :test-param1 200)
        (is (get-entity-param entity :test-param1) 200))))
  (subtest "errors"
    (with-prove-in-both ()
      (is-error (init-entity-params :x 0 :without-value)
                'simple-error))))


;; TODO: Test script system.
;;    :script-2d
;;    :script-2d-func
;;    :make-script-system

(finalize)
