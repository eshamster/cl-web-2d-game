(defpackage cl-web-2d-game/t/collision-system
  (:use :cl
        :rove
        :cl-ps-ecs
        :ps-experiment/t/test-utils 
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/physics/collision-system
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/utils/calc
        :cl-web-2d-game/t/test-utils)
  (:import-from :cl-ps-ecs/t/test-utils
                :with-ecs-env)
  (:import-from :ps-experiment
                :defun.ps+
                :defmacro.ps+
                :defvar.ps+))
(in-package :cl-web-2d-game/t/collision-system)

;; --- utils --- ;;

;; Note: One collision increments the count by 2.
(defvar.ps+ *collision-count* 0)

(defmacro.ps+ with-collision-system (&body body)
  `(unwind-protect
        (with-ecs-env ()
          (register-ecs-system "collision" (make-collision-system))
          ;; model is not supported in CL environment.
          (setf-collider-model-enable nil)
          ,@body)
     (setf *collision-count* 0)))

(defun.ps+ process-one-frame ()
  (ecs-main))

;; --- entities --- ;;

(defun.ps+ make-collision-test-entity
    (&key (check-mine-and-enemy-p nil)
          x
          (tags (list))
          (target-tags (list)))
  (let ((entity (make-ecs-entity)))
    (dolist (tag tags)
      (add-entity-tag entity tag))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y 0)
     (make-physic-circle
      :r 100
      :target-tags target-tags
      :on-collision (lambda (mine enemy)
                      (when check-mine-and-enemy-p
                        (ok (eq entity mine))
                        (ok (not (eq entity enemy)))
                        (ok (typep enemy 'ecs-entity)))
                      (incf *collision-count*))))
    entity))

;; Note:
;; - same types collide
;; - different types don't collide

(defun.ps+ make-entity-type1
    (&key (check-mine-and-enemy-p nil)
          (tags (list))
          (target-tags (list)))
  (make-collision-test-entity
   :check-mine-and-enemy-p check-mine-and-enemy-p
   :x 0
   :tags tags
   :target-tags target-tags))

(defun.ps+ make-entity-type2 ()
  (make-collision-test-entity
   :x 1000))

;; --- test --- ;;

;; TODO: Enable JS test

(deftest basic-behaviors
  (testing "mine is mine, enemy is not mine but entity"
    (with-collision-system
      (add-ecs-entity (make-entity-type1
                       :check-mine-and-enemy-p t))
      (add-ecs-entity (make-entity-type1))
      (process-one-frame)))
  (testing "should not collision by self"
    (with-collision-system
      (add-ecs-entity (make-entity-type1))
      (process-one-frame)
      (ok (= *collision-count* 0))))
  (testing "case where all entities collide each other"
    (with-collision-system
      (add-ecs-entity (make-entity-type1))
      (add-ecs-entity (make-entity-type1))
      (process-one-frame)
      (ok (= *collision-count* 2)))
    (with-collision-system
      (add-ecs-entity (make-entity-type1))
      (add-ecs-entity (make-entity-type1))
      (add-ecs-entity (make-entity-type1))
      (process-one-frame)
      (ok (= *collision-count* 6))))
  (testing "case where some of entities collide"
    (with-collision-system
      (add-ecs-entity (make-entity-type1))
      (add-ecs-entity (make-entity-type1))
      (add-ecs-entity (make-entity-type2))
      (add-ecs-entity (make-entity-type2))
      (process-one-frame)
      (ok (= *collision-count* 4)))))

(defun.ps+ test-target-tags (target-tags1 tags1 target-tags2 tags2
                                          should-collide-p)
  (with-collision-system
    (add-ecs-entity (make-entity-type1 :target-tags target-tags1
                                       :tags tags1))
    (add-ecs-entity (make-entity-type1 :target-tags target-tags2
                                       :tags tags2))
    (process-one-frame)
    (if should-collide-p
        (ok (= *collision-count* 2))
        (ok (= *collision-count* 0)))))

(deftest collision-considering-tag
  (testing "case where enitities have no target tag"
    (skip "Skip: It has been done in the above tests"))
  (testing "case where only one entity has target tag"
    (test-target-tags '(:a) '()
                      '() '()
                      nil)
    (test-target-tags '(:a) '()
                      '() '(:a)
                      t))
  (testing "case where only both entities have target tag"
    (test-target-tags '(:a) '()
                      '(:b) '()
                      nil)
    (test-target-tags '(:a) '(:b)
                      '(:b) '(:a)
                      t))
  (testing "case of multiple target tags"
    (test-target-tags '(:a :b) '()
                      '() '(:c)
                      nil)
    (test-target-tags '(:a :b) '()
                      '() '(:b)
                      t)))
