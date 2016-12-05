(in-package :cl-user)
(defpackage cl-web-2d-game.input
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.camera
        :parenscript)
  (:export :add-mouse-down-callback
           :add-mouse-up-callback
           :add-mouse-move-callback
           :add-touch-start-callback
           :add-touch-end-callback
           :add-touch-move-callback

           :mouse-event-x
           :mouse-event-y

           :touch-event-touches
           :touch-event-element-x
           :touch-event-element-y

           :initialize-input))
(in-package :cl-web-2d-game.input)

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

;; ---- keyboard ---- ;;

(defvar.ps keyboard (new (#j.THREEx.KeyboardState#)))
(defvar.ps key-status (make-hash-table))

(defun.ps is-key-down (keyname)
  "Return if the key is down"
  (let ((value (gethash (keyboard.keyname-to-keycode keyname) key-status)))
    (and (not (null value))
         (or (eq value :down) (eq value :down-now)))))

(defun.ps is-key-down-now (keyname)
  "Return if the key is down just in this frame"
  (let ((value (gethash (keyboard.keyname-to-keycode keyname) key-status)))
    (and (not (null value))
         (eq value :down-now))))

(defun.ps is-key-up-now (keyname)
  "Return if the key is down just in this frame"
  (let ((value (gethash (keyboard.keyname-to-keycode keyname) key-status)))
    (and (not (null value))
         (eq value :up-now))))

(defun.ps process-keyboard-input ()
  (maphash (lambda (k v)
             (setf (gethash k key-status)
                   (calc-next-input-state (gethash k key-status) v)))
           keyboard.key-codes))

;; ---- mouse ---- ;;

;; variables

(defvar.ps+ _mouse-x -100)
(defvar.ps+ _mouse-y -100)
(defvar.ps+ _mouse-left :up)

(defvar.ps+ *mouse-x-buffer* -100)
(defvar.ps+ *mouse-y-buffer* -100)
(defvar.ps+ *mouse-left-buffer* nil)

(defvar.ps+ +mouse-left-button-id+ 1)
(defvar.ps+ +mouse-right-button-id+ 3)

;; main

(defun.ps+ process-mouse-input ()
  (setf _mouse-x *mouse-x-buffer*)
  (setf _mouse-y *mouse-y-buffer*)
  (setf _mouse-left
        (calc-next-input-state _mouse-left
                               *mouse-left-buffer*)))

;; interfaces

(defun.ps+ get-mouse-x () _mouse-x)
(defun.ps+ get-mouse-y () _mouse-y)
(defun.ps+ get-left-mouse-state () _mouse-left)

;; (private)
(defun.ps set-mouse-point (x y)
  (let* ((renderer (document.query-selector "#renderer"))
         (canvas (renderer.query-selector "canvas")))
    (setf *mouse-x-buffer* (- x renderer.offset-left
                              (get-camera-offset-x)))
    (setf *mouse-y-buffer* (- canvas.height
                              (- y renderer.offset-top)
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
  (call-mouse-down-callbacks (init-mouse-event e)))

(defun.ps on-mouse-up-event (e)
  (when (= e.which *mouse-left-button-id*)
    (setf +mouse-left-buffer+ nil))
  (call-mouse-up-callbacks (init-mouse-event e)))

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

(defun.ps initialize-input ()
  (window.add-event-listener "mousemove" on-mouse-move-event)
  (window.add-event-listener "mousedown" on-mouse-down-event)
  (window.add-event-listener "mouseup" on-mouse-up-event)
  (window.add-event-listener "touchstart" on-touch-start)
  (window.add-event-listener "touchend" on-touch-end)
  (window.add-event-listener "touchmove" on-touch-move-event)
  (window.add-event-listener "keydown" (lambda (e) (e.prevent-default))))
