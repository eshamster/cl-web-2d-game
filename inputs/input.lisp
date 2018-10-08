(in-package :cl-user)
(defpackage cl-web-2d-game/inputs/input
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/camera
        :parenscript)
  (:export :add-mouse-down-callback
           :add-mouse-up-callback
           :add-mouse-move-callback
           :add-mouse-wheel-callback
           :add-touch-start-callback
           :add-touch-end-callback
           :add-touch-move-callback

           :start-key-monitoring
           :key-down-now-p
           :key-down-p
           :key-up-now-p
           :key-up-p
           :key-down-count
           :key-up-count
           :get-physical-key-name

           :get-mouse-x
           :get-mouse-y
           :get-left-mouse-state
           :get-right-mouse-state
           :get-mouse-wheel-delta-y
           :get-mouse-state
           :get-mouse-down-count
           :get-mouse-up-count

           :mouse-event-x
           :mouse-event-y

           :init-input
           :process-input

           :do-touch-state
           :get-touch-state
           :get-touch-x
           :get-touch-y
           :get-total-touch-state
           :get-total-touch-x
           :get-total-touch-y

           ;; obsoleted
           :is-key-down
           :is-key-down-now
           :is-key-up
           :is-key-up-now)
  (:import-from :cl-web-2d-game/core/basic-components
                :make-vector-2d
                :vector-2d-x
                :vector-2d-y)
  (:import-from :cl-web-2d-game/utils/dom-manager
                :get-rendered-dom)
  (:import-from :cl-web-2d-game/utils/utils
                :def-obsoleted-alias.ps+)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-web-2d-game/inputs/input)

(enable-ps-experiment-syntax)

(defun.ps process-input ()
  (process-keyboard-input)
  (process-mouse-input)
  (process-touch-input))

;; --- common --- ;;

(defun.ps+ calc-next-input-count (current device-state)
  (if device-state
      (if (>= current 0) (1+ current) 1)
      (if (<= current 0) (1- current) -1)))

(defun.ps+ calc-state-from-count (count)
  (cond ((= count 1) :down-now)
        ((> count 1) :down)
        ((= count -1) :up-now)
        ((< count -1) :up)
        (;; Note: This case can occur only at the first frame.
         (= count 0) :up)))

(defun.ps+ input-on-now-p (current)
  (= current 1))
(defun.ps+ input-on-p (current)
  (> current 0))
(defun.ps+ input-off-now-p (current)
  (= current -1))
(defun.ps+ input-off-p (current)
  (< current 0))

(defun.ps+ input-on-count (current)
  (if (input-on-p current) current 0))
(defun.ps+ input-off-count (current)
  (if (input-off-p current) (* -1 current) 0))

;; ---- keyboard ---- ;;

(defvar.ps *keyboard* (new (#j.THREEx.KeyboardState#)))

(defvar.ps+ *button-to-keyboard* (make-hash-table))
(defvar.ps+ *key-status* (make-hash-table))

(defun.ps+ start-key-monitoring (virtual-key-name physical-key-name)
  "Start monitoring the key \"physical-key-name\". It will be accessed by is-key-down or etc with \"virutal-ke-name\"."
  (setf (gethash virtual-key-name *button-to-keyboard*) physical-key-name
        (gethash virtual-key-name *key-status*) 0))

;; TODO: stop-key-monitoring

(defun.ps+ init-keyboard ()
  (mapcar (lambda (pair)
            (start-key-monitoring (car pair) (cadr pair)))
          '((:a "z")
            (:b "x")
            (:c "c")
            (:left  "left")
            (:right "right")
            (:up    "up")
            (:down  "down"))))

(defun.ps+ get-physical-key-name (virtual-key-name)
  (gethash virtual-key-name *button-to-keyboard*))

(defun.ps+ key-down-p (button)
  "Return if the button is down"
  (input-on-p (gethash button *key-status*)))

(defun.ps+ key-down-now-p (button)
  "Return if the button is down just in this frame"
  (input-on-now-p (gethash button *key-status*)))

(defun.ps+ key-up-p (button)
  "Return if the button is up"
  (input-off-p (gethash button *key-status*)))

(defun.ps+ key-up-now-p (button)
  "Return if the button is up just in this frame"
  (input-off-now-p (gethash button *key-status*)))

(def-obsoleted-alias.ps+ is-key-down-now key-down-now-p)
(def-obsoleted-alias.ps+ is-key-down     key-down-p)
(def-obsoleted-alias.ps+ is-key-up-now   key-up-now-p)
(def-obsoleted-alias.ps+ is-key-up       key-up-p)

(defun.ps+ key-down-count (button)
  (input-on-count (gethash button *key-status*)))
(defun.ps+ key-up-count (button)
  (input-off-count (gethash button *key-status*)))

(defun.ps process-keyboard-input ()
  (maphash (lambda (button key)
             (symbol-macrolet ((input-count (gethash button *key-status*)))
               (setf input-count
                     (calc-next-input-count input-count
                                            (*keyboard*.pressed key)))))
           *button-to-keyboard*))

;; ---- mouse ---- ;;

;; variables

(defvar.ps+ _mouse-x -100)
(defvar.ps+ _mouse-y -100)
(defvar.ps+ *mouse-left-count* 0)
(defvar.ps+ *mouse-right-count* 0)

(defvar.ps+ *mouse-x-buffer* -100)
(defvar.ps+ *mouse-y-buffer* -100)
(defvar.ps+ *mouse-left-buffer* nil)
(defvar.ps+ *mouse-right-buffer* nil)

(defvar.ps+ +mouse-left-button-id+ 1)
(defvar.ps+ +mouse-right-button-id+ 3)

(defvar.ps+ *mouse-wheel-delta-y-buffer* 0)
(defvar.ps+ *mouse-wheel-delta-y* 0)

;; main

(defun.ps+ process-mouse-input ()
  (setf _mouse-x *mouse-x-buffer*)
  (setf _mouse-y *mouse-y-buffer*)
  (setf *mouse-left-count*
        (calc-next-input-count *mouse-left-count* *mouse-left-buffer*))
  (setf *mouse-right-count*
        (calc-next-input-count *mouse-right-count* *mouse-right-buffer*))
  (setf *mouse-wheel-delta-y* *mouse-wheel-delta-y-buffer*
        *mouse-wheel-delta-y-buffer* 0))

;; interfaces

(defun.ps+ get-mouse-x () _mouse-x)
(defun.ps+ get-mouse-y () _mouse-y)
(defun.ps+ get-left-mouse-state ()
  (calc-state-from-count *mouse-left-count*))
(defun.ps+ get-right-mouse-state ()
  (calc-state-from-count *mouse-right-count*))
(defun.ps+ get-mouse-wheel-delta-y () *mouse-wheel-delta-y*)

(defun.ps+ get-mouse-state (which)
  "Get mouse state. \"which\" means :left or :right."
  (ecase which
    (:left (get-left-mouse-state))
    (:right (get-right-mouse-state))))
(defun.ps+ get-mouse-down-count (which)
  (input-on-count
   (ecase which
     (:left *mouse-left-count*)
     (:right *mouse-right-buffer*))))
(defun.ps+ get-mouse-up-count (which)
  (input-off-count
   (ecase which
     (:left *mouse-left-count*)
     (:right *mouse-right-buffer*))))

(defun.ps make-adjusted-input-point (x y)
  (let* ((renderer (get-rendered-dom))
         (canvas (renderer.query-selector "canvas"))
         (scale (/ (* 1.0 renderer.client-height) (get-screen-height))))
    (make-vector-2d :x (- (/ (- x renderer.offset-left)
                             scale)
                          (get-camera-offset-x))
                    :y (- (/ (+ (- canvas.height y) renderer.offset-top)
                             scale)
                          (get-camera-offset-y)))))

(defun.ps set-mouse-point (x y)
  (let ((adjusted (make-adjusted-input-point x y)))
    (setf *mouse-x-buffer* (vector-2d-x adjusted)
          *mouse-y-buffer* (vector-2d-y adjusted))))

;; --- self callbacks --- ;;

(defmacro.ps+ def-input-callback (name)
  "Ex. when name = mouse-move, *MOUSE-MOVE-CALLBACKS*, ADD-MOUSE-MOVE-CALLBACK and CALL-MOUSE-MOVE-CALLBACKS are defined."
  (let ((list-sym (intern (format nil "*~A-CALLBACKS*" name))))
    `(progn (defvar.ps+ ,list-sym '())
            (defun.ps+ ,(intern (format nil "ADD-~A-CALLBACK" name)) (callback)
              (push callback ,list-sym))
            (defun.ps+ ,(intern (format nil "CALL-~A-CALLBACKS" name)) (e)
              (dotimes (i (length ,list-sym))
                (funcall (aref ,list-sym i) e))))))

(def-input-callback mouse-down)
(def-input-callback mouse-up)
(def-input-callback mouse-move)

(def-input-callback mouse-wheel)

(def-input-callback touch-start)
(def-input-callback touch-end)
(def-input-callback touch-move)

;; --- javascript callbacks --- ;;

;; mouse

(defstruct.ps+ mouse-event x y)

(defun.ps init-mouse-event (e)
  (make-mouse-event :x e.client-x :y e.client-y))

(defun.ps on-mouse-move-event (e)
  (set-mouse-point e.client-x e.client-y)
  (call-mouse-move-callbacks (init-mouse-event e)))

(defun.ps on-mouse-down-event (e)
  (when (= e.which +mouse-left-button-id+)
    (setf *mouse-left-buffer* t))
  (when (= e.which +mouse-right-button-id+)
    (setf *mouse-right-buffer* t))
  (call-mouse-down-callbacks (init-mouse-event e)))

(defun.ps on-mouse-up-event (e)
  (when (= e.which +mouse-left-button-id+)
    (setf *mouse-left-buffer* nil))
  (when (= e.which +mouse-right-button-id+)
    (setf *mouse-right-buffer* nil))
  (call-mouse-up-callbacks (init-mouse-event e)))

;; mouse wheel

(defun.ps on-wheel-event (e)
  (setf *mouse-wheel-delta-y-buffer* e.delta-y)
  (call-mouse-wheel-callbacks e))

;; touch

;; Note: About count in touch-event-element:
;; A count less than 0 is interpretted as released.
;; If the count is 0, it can not been seen from out of this package.
;; For example, it is skipped in do-touch-state macro.
;; This is because a touch-event-element is added independently of game frame,
;; so we can't assure that the state where count is 0 continues in a full frame.
;; By the same reason, if the count is -1, it is interpretted as :down.

(defstruct.ps+ touch-event-element x y id (count 0))
(defstruct.ps+ touch-event touches)

(defvar.ps+ *touch-state-hash* (make-hash-table))
(defun.ps+ get-touch-state-hash () *touch-state-hash*)

(defmacro.ps+ do-touch-state ((var-id) &body body)
  (with-gensyms (hash-value)
    `(maphash (lambda (,var-id ,hash-value)
                (declare (ignorable ,var-id))
                (unless (= (touch-event-element-count ,hash-value) 0)
                  ,@body))
              (get-touch-state-hash))))

(defun.ps+ get-touch-state (id)
  (let ((elem (gethash id *touch-state-hash*)))
    (unless elem
      (return-from get-touch-state :up))
    (let ((count (touch-event-element-count elem)))
      (cond ((= count 0) :up) ; ignored (please see the above note for detail)
            ((= count 1) :down-now)
            ((> count 1) :down)
            ((= count -1) :down)
            ((= count -2) :up-now)
            (t :up) ; (< count -2)
            ))))

(defun.ps+ get-total-touch-state ()
  (let ((count 0)
        result)
    (do-touch-state (id)
      (incf count))
    (case count
      (0 (setf result :up))
      (1 (do-touch-state (id)
           (setf result (get-touch-state id))))
      (t (labels ((calc-priority (state)
                    (ecase state
                      (:down 4) (:down-now 3)
                      (:up-now 2) (:up 1)))
                  (prior-p (new-state old-state)
                    (> (calc-priority new-state)
                       (calc-priority old-state))))
           (do-touch-state (id)
             (let ((state (get-touch-state id)))
               (when (or (null result)
                         (prior-p state result))
                 (setf result state)))))))
    (assert result)
    result))

(defun.ps+ get-touch-x (id)
  (let ((elem (gethash id *touch-state-hash*)))
    (assert (and elem
                 (> (touch-event-element-count elem) 0)))
    (touch-event-element-x elem)))

(defun.ps+ get-total-touch-average (fn default-value)
  (let ((sum 0)
        (count 0))
    (do-touch-state (id)
      (incf sum (funcall fn id))
      (incf count))
    (if (> count 0)
        (/ sum count)
        default-value)))

(defun.ps+ get-total-touch-x ()
  (get-total-touch-average #'get-touch-x 0))

(defun.ps+ get-touch-y (id)
  (let ((elem (gethash id *touch-state-hash*)))
    (assert (and elem
                 (> (touch-event-element-count elem) 0)))
    (touch-event-element-y elem)))

(defun.ps+ get-total-touch-y ()
  (get-total-touch-average #'get-touch-y 0))

(defun.ps+ process-touch-input ()
  (maphash (lambda (id state)
             (with-slots (count) state
               (cond ((>= count 0)
                      (incf count))
                     (t (decf count)
                        (when (< count -2)
                          (register-next-frame-func
                           (lambda () (remhash id *touch-state-hash*))))))))
           *touch-state-hash*))

(defun.ps set-xy-of-touch-event-element (elem raw-touch-event)
  (let ((adjusted (make-adjusted-input-point raw-touch-event.client-x
                                             raw-touch-event.client-y)))
    (setf (touch-event-element-x elem) (vector-2d-x adjusted)
          (touch-event-element-y elem) (vector-2d-y adjusted))))

(defun.ps+ update-touch-state-by-event (id touch-event)
  (let ((target (gethash id *touch-state-hash*)))
    (assert target)
    (set-xy-of-touch-event-element target touch-event)))

(defun.ps init-touch-event (e)
  (let* ((result (make-touch-event :touches (make-array e.touches.length)))
         (touches (touch-event-touches result)))
    (dotimes (i e.changed-touches.length)
      (let* ((touch (aref e.changed-touches i))
             (elem (make-touch-event-element :id touch.identifier)))
        (set-xy-of-touch-event-element elem touch)
        (setf (aref touches i) elem)))
    result))

(defun.ps+ on-touch-start (e)
  (let ((event (init-touch-event e)))
    (dolist (event-elem (touch-event-touches event))
      (setf (gethash (touch-event-element-id event-elem) *touch-state-hash*)
            event-elem))
    (call-touch-start-callbacks event)))

(defun.ps on-touch-end (e)
  (when (= e.touches.length 0)
    ;; [WIP]
    )
  (dolist (touch e.changed-touches)
    (let ((event-elem (gethash touch.identifier *touch-state-hash*)))
      (assert event-elem)
      (setf (touch-event-element-count event-elem) -1)))
  (call-touch-end-callbacks (init-touch-event e)))

(defun.ps on-touch-move-event (e)
  (dolist (touch e.changed-touches)
    (let ((event-elem (gethash touch.identifier *touch-state-hash*)))
      (assert event-elem)
      (update-touch-state-by-event (touch-event-element-id event-elem) touch)))
  (call-touch-move-callbacks (init-touch-event e)))

;; register

(defun.ps init-input ()
  (init-keyboard)
  (window.add-event-listener "contextmenu" (lambda (e) (e.prevent-default)))
  (window.add-event-listener "mousemove" on-mouse-move-event)
  (window.add-event-listener "mousedown" on-mouse-down-event)
  (window.add-event-listener "mouseup" on-mouse-up-event)
  (window.add-event-listener "wheel" on-wheel-event)
  (window.add-event-listener "touchstart" on-touch-start)
  (window.add-event-listener "touchend" on-touch-end)
  (window.add-event-listener "touchmove" on-touch-move-event)
  (window.add-event-listener "keydown" (lambda (e) (e.prevent-default))))
