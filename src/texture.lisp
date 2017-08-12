(in-package :cl-user)
(defpackage cl-web-2d-game.texture
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.basic-components
        :cl-web-2d-game.logger)
  (:export :texture-2d
           :texture-2d-p
           :texture-2d-material
           :texture-2d-rect-uv
           :load-texture
           :unload-texture
           :get-texture-async))
(in-package :cl-web-2d-game.texture)

(enable-ps-experiment-syntax)

(defstruct.ps+ texture-2d (path-list '()) (name "") material
               (rect-uv (make-rect-2d)))

(defstruct.ps+ raw-image-bitmap promise (ref-count 0))

(defvar.ps+ *texture-table* '())

(defvar.ps+ *raw-image-bitmap-table* (make-hash-table))

(defun.ps find-raw-image-bitmap (path)
  ;; Note: As Common Lisp code, ":test #'string=" is required.
  ;; But ":test" is not implemented for Parenscript...
  (gethash path *raw-image-bitmap-table*))

(defun.ps remove-raw-image-bitmap (path material)
  (console-log :loader :debug
               "Remove raw-image-bitmap (path = ~A)" path)
  (material.map.dispose)
  (setf (gethash path *raw-image-bitmap-table*)
        nil))

(defun.ps find-texture (name)
  (find-if (lambda (wrapper)
             (string= (texture-2d-name wrapper)
                      name))
           *texture-table*))

(defun.ps get-load-texture-promise (&key path loader)
  (let ((raw (find-raw-image-bitmap path)))
    (if raw
        (with-slots (ref-count) raw
          (incf ref-count)
          (console-log :loader :debug
                       "Skip loading ~A (Increase ref-count to ~D)"
                       path ref-count)
          (raw-image-bitmap-promise raw))
        (progn
          (when path
            (console-log :loader :debug
                         "Start loading ~A" path))
          (setf start-time (performance.now))
          (let ((promise
                 (new (-promise
                       (lambda (resolve)
                         (if path
                             (loader.load path
                                          (lambda (image-bitmap)
                                            (console-log :loader :debug
                                                         "Time to load texture ~A: ~F ms" path
                                                         (- (performance.now) start-time))
                                            (resolve image-bitmap)))
                             (resolve nil)))))))
            (setf (gethash path *raw-image-bitmap-table*)
                  (make-raw-image-bitmap :promise promise
                                         :ref-count 1))
            promise)))))

(defun.ps load-texture (&key path name (alpha-path nil)
                             (x 0) (y 0) (width 1.0) (height 1.0))
  "Asynchronously Load texture by path and register it by name"
  ;; TODO: Unload a registred texture that has the same name if exists.
  (push (make-texture-2d :name name
                         :path-list (if alpha-path
                                        (list path alpha-path)
                                        (list path))
                         :material nil
                         :rect-uv (make-rect-2d :x x :y y
                                                :width width :height height))
        *texture-table*)
  (let* ((loader (new (#j.THREE.TextureLoader#)))
         (start-time nil)
         (promise-main (get-load-texture-promise :path path
                                                 :loader loader))
         (promise-alpha (get-load-texture-promise :path alpha-path
                                                  :loader loader)))
    (flet ((load-callback (image-bitmap alpha-bitmap)
             (let ((tex (find-texture name)))
               (unless tex
                 (error "The find-texture should be successed"))
               (setf (texture-2d-material tex)
                     (new (#j.THREE.MeshBasicMaterial#
                           (create map image-bitmap
                                   alpha-map alpha-bitmap
                                   transparent (if alpha-bitmap true false)
                                   color #xffffff))))))))
    ((@ (-promise.all (list promise-main promise-alpha)) then)
     (lambda (values)
       (load-callback (nth 0 values) (nth 1 values))))))

;; Note: not tested function
(defun.ps unload-texture (name)
  (let ((tex (find-texture name)))
    (unless tex
      (error "The texture \"~A\" is not loaded." name))
    (setf *texture-table* (remove tex *texture-table*))
    (dolist (path (texture-2d-path-list tex))
      (let ((raw (find-raw-image-bitmap path)))
        (unless raw
          (error "The path \"~A\" is not loaded." path))
        (with-slots (ref-count) raw
          (if (= ref-count 1)
              (remove-raw-image-bitmap path
                                       (texture-2d-material tex))
              (progn (decf ref-count)
                     (console-log :loader :debug
                                  "Decrease the ref-count of ~A to ~D"
                                  path ref-count))))))))

(defvar.ps+ *load-texture-timeout-frames* 120)

(defun.ps+ get-texture-async (name callback)
  (let ((tex (find-texture name)))
    (unless tex
      (error "The texture \"~A\" is not loaded." name))
    (if (texture-2d-material tex)
        (funcall callback tex)
        (register-func-with-pred
         (lambda () (funcall callback tex))
         (lambda () (texture-2d-material tex))
         :timeout-frame *load-texture-timeout-frames*))))

