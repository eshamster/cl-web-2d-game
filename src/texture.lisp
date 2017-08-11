(in-package :cl-user)
(defpackage cl-web-2d-game.texture
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game.logger)
  (:export :texture-2d
           :texture-2d-p
           :texture-2d-material
           :texture-2d-uv
           :load-texture
           :unload-texture
           :get-texture-async
           :make-rect-uvs))
(in-package :cl-web-2d-game.texture)

(enable-ps-experiment-syntax)

;; TODO: Enable to process multiple texture in one image

(defstruct.ps+ texture-2d (name "") material
               (uv (make-rect-uvs 0 0 1.0 1.0)))

(defvar.ps+ *texture-load-timeout-ms* 2000)

(defvar.ps+ *texture-table* '())

(defun.ps find-texture (name)
  (find-if (lambda (wrapper)
             (string= (texture-2d-name wrapper)
                      name))
           *texture-table*))

(defun.ps load-texture (&key path name (alpha-path nil))
  "Asynchronously Load texture by path and register it by name"
  ;; TODO: Unload a registred texture that has the same name if exists.
  (push (make-texture-2d :name name
                         :material nil)
        *texture-table*)
  (let* ((loader (new (#j.THREE.TextureLoader#)))
         (start-time nil)
         (promise-alpha
          (new (-promise
                (lambda (resolve)
                  (if alpha-path
                      (loader.load alpha-path
                                   (lambda (image-bitmap)
                                     (resolve image-bitmap)))
                      (resolve nil)))))))
    (flet ((load-callback (image-bitmap)
             (let ((tex (find-texture name)))
               (unless tex
                 (error "The find-texture should be successed"))
               (promise-alpha.then
                (lambda (alpha-bitmap)
                  (setf (texture-2d-material tex)
                        (new (#j.THREE.MeshBasicMaterial#
                              (create map image-bitmap
                                      alpha-map alpha-bitmap
                                      transparent (if alpha-bitmap true false)
                                      color 0xffffff))))))))))
    (setf start-time (performance.now))
    (loader.load path
                 (lambda (image-bitmap)
                   (console-log :loader :debug
                                "Time to load texture ~A: ~F ms"
                                path
                                (- (performance.now) start-time))
                   (load-callback image-bitmap)))))

(defun.ps unload-texture (name)
  (let ((tex (find-texture name)))
    (unless tex
      (error "The texture \"~A\" is not loaded." name))
    (with-slots (material) tex
      (material.map.dispose))
    (setf *texture-table* (remove tex *texture-table*)))) 

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

;; --- UV functions --- ;;

(defun.ps make-rect-uvs (x y width height)
  (flet ((new-uv (u v)
           (new (#j.THREE.Vector2# u v))))
    (list (list (new-uv x y)
                (new-uv (+ x width) y)
                (new-uv (+ x width) (+ y height)))
          (list (new-uv (+ x width) (+ y height))
                (new-uv x (+ y height))
                (new-uv x y)))))
