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

           :is-key-down
           :is-key-down-now
           :is-key-up
           :is-key-up-now
           :key-down-count
           :key-up-count

           :get-mouse-x
           :get-mouse-y
           :get-left-mouse-state
           :get-right-mouse-state
           :get-mouse-wheel-delta-y

           :mouse-event-x
           :mouse-event-y

           :touch-event-touches
           :touch-event-element-x
           :touch-event-element-y

           :init-input
           :process-input)
  (:import-from :cl-web-2d-game/utils/dom-manager
                :get-rendered-dom))
(in-package :cl-web-2d-game/inputs/input)

(enable-ps-experiment-syntax)

(defun.ps process-input ()
  (process-keyboard-input)
  (process-mouse-input))

;; --- common --- ;;

(defun.ps+ calc-next-input-state (now-state device-state)
  "now-state = :down-now | :down | :up-now | :up
device-state = boolean-value"
  (if device-state
      (case now-state
        ((:down-now :down) :down)
        (t :down-now))
      (case now-state
        ((:up-now :up) :up)
        (t :up-now))))

(defun.ps+ calc-next-input-count (current device-state)
  (if device-state
      (if (>= current 0) (1+ current) 1)
      (if (<= current 0) (1- current) -1)))

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

(defvar.ps keyboard (new (#j.THREEx.KeyboardState#)))

(defvar.ps *button-to-keyboard*
    (create :a "z"
            :b "x"
            :c "c"
            :left  "left"
            :right "right"
            :up    "up"
            :down  "down"))
(defvar.ps+ *key-status* (make-hash-table))

;; TODO: Rename function names according to Common Lisp tradition
;; (Ex. is-key-down -> key-down-p)

(defun.ps+ is-key-down (button)
  "Return if the button is down"
  (input-on-p (gethash button *key-status*)))

(defun.ps+ is-key-down-now (button)
  "Return if the button is down just in this frame"
  (input-on-now-p (gethash button *key-status*)))

(defun.ps+ is-key-up (button)
  "Return if the button is up"
  (input-off-p (gethash button *key-status*)))

(defun.ps+ is-key-up-now (button)
  "Return if the button is up just in this frame"
  (input-off-now-p (gethash button *key-status*)))

(defun.ps+ key-down-count (button)
  (input-on-count (gethash button *key-status*)))
(defun.ps+ key-up-count (button)
  (input-off-count (gethash button *key-status*)))

(defun.ps process-keyboard-input ()
  (maphash (lambda (button key)
             (setf (gethash button *key-status*)
                   (calc-next-input-count (let ((value (gethash button *key-status*)))
                                             (if value value 0))
                                          (keyboard.pressed key))))
           *button-to-keyboard*))

;; ---- mouse ---- ;;

;; variables

(defvar.ps+ _mouse-x -100)
(defvar.ps+ _mouse-y -100)
(defvar.ps+ _mouse-left :up)
(defvar.ps+ _mouse-right :up)

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
  (setf _mouse-left
        (calc-next-input-state _mouse-left
                               *mouse-left-buffer*))
  (setf _mouse-right
        (calc-next-input-state _mouse-right
                               *mouse-right-buffer*))
  (setf *mouse-wheel-delta-y* *mouse-wheel-delta-y-buffer*
        *mouse-wheel-delta-y-buffer* 0))

;; interfaces

(defun.ps+ get-mouse-x () _mouse-x)
(defun.ps+ get-mouse-y () _mouse-y)
(defun.ps+ get-left-mouse-state () _mouse-left)
(defun.ps+ get-right-mouse-state () _mouse-right)
(defun.ps+ get-mouse-wheel-delta-y () *mouse-wheel-delta-y*)

;; (private)
(defun.ps set-mouse-point (x y)
  (let* ((renderer (get-rendered-dom))
         (canvas (renderer.query-selector "canvas"))
         (scale (/ (* 1.0 renderer.client-height) (get-screen-height))))
    (setf *mouse-x-buffer* (- (/ (- x renderer.offset-left)
                                 scale)
                              (get-camera-offset-x)))
    (setf *mouse-y-buffer* (- (/ (+ (- canvas.height y) renderer.offset-top)
                                 scale)
                              (get-camera-offset-y)))))

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
  (when (= e.which *mouse-left-button-id*)
    (setf +mouse-left-buffer+ nil))
  (when (= e.which *mouse-right-button-id*)
    (setf +mouse-right-buffer+ nil))
  (call-mouse-up-callbacks (init-mouse-event e)))

;; mouse wheel

(defun.ps on-wheel-event (e)
  (setf *mouse-wheel-delta-y-buffer* e.delta-y)
  (call-mouse-wheel-callbacks e))

;; touch

(defstruct.ps+ touch-event-element x y)
(defstruct.ps+ touch-event touches)

(defun.ps init-touch-event (e)
  (let* ((result (make-touch-event :touches (make-array e.touches.length)))
         (touches (touch-event-touches result)))
    (dotimes (i e.touches.length)
      (let ((point (aref e.touches i)))
        (setf (aref touches i)
              (make-touch-event-element :x *mouse-x-buffer* :y *mouse-y-buffer*))))
    result))

(defun.ps set-point-by-touch (e)
  (let ((point (aref e.touches 0)))
    (set-mouse-point point.client-x point.client-y)))

(defun.ps on-touch-start (e)
  (set-point-by-touch e)
  (setf *mouse-left-buffer* t)
  (call-touch-start-callbacks (init-touch-event e)))

(defun.ps on-touch-end (e)
  (when (= e.touches.length 0)
    (setf *mouse-left-buffer* nil))
  (call-touch-end-callbacks (init-touch-event e)))

(defun.ps on-touch-move-event (e)
  (set-point-by-touch e)
  (call-touch-move-callbacks (init-touch-event e)))

;; register

(defun.ps init-input ()
  (window.add-event-listener "contextmenu" (lambda (e) (e.prevent-default)))
  (window.add-event-listener "mousemove" on-mouse-move-event)
  (window.add-event-listener "mousedown" on-mouse-down-event)
  (window.add-event-listener "mouseup" on-mouse-up-event)
  (window.add-event-listener "wheel" on-wheel-event)
  (window.add-event-listener "touchstart" on-touch-start)
  (window.add-event-listener "touchend" on-touch-end)
  (window.add-event-listener "touchmove" on-touch-move-event)
  (window.add-event-listener "keydown" (lambda (e) (e.prevent-default))))
