(in-package :cl-user)
(defpackage cl-web-2d-game/graphics/texture
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/utils/debug/logger)
  (:export :texture-2d
           :texture-2d-p
           :texture-2d-material
           :texture-2d-rect-uv
           :load-texture
           :unload-texture
           :get-texture-promise

           :get-texture-2d-width
           :get-texture-2d-height))
(in-package :cl-web-2d-game/graphics/texture)

(enable-ps-experiment-syntax)

(defstruct.ps+ texture-2d (path-list '()) material
               (rect-uv (make-rect-2d)))

(defstruct.ps+ raw-image-bitmap promise (ref-count 0))

;; A bitmap can include multiple textures. So the former is managed in *raw-image-bitmap-table*
;; and the latter is managed in *texture-promise-table*
(defvar.ps+ *texture-promise-table* (make-hash-table))

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

(defun.ps find-texture-promise (name)
  (gethash name *texture-promise-table*))

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
                 (init-frame-promise
                  (lambda (resolve)
                    (if path
                        (loader.load (if (path.starts-with "/") (+ "." path) path)
                                     (lambda (image-bitmap)
                                       (console-log :loader :debug
                                                    "Time to load texture ~A: ~F ms" path
                                                    (- (performance.now) start-time))
                                       (resolve image-bitmap)))
                        (resolve nil))))))
            (setf (gethash path *raw-image-bitmap-table*)
                  (make-raw-image-bitmap :promise promise
                                         :ref-count 1))
            promise)))))

(defvar.ps+ *load-texture-timeout-frames* 120)

(defun.ps load-texture (&key path name (alpha-path nil)
                             (x 0) (y 0) (width 1.0) (height 1.0))
  "Asynchronously Load texture by path and register it by name"
  ;; TODO: Unload a registred texture that has the same name if exists.
  (let* ((loader (new (#j.THREE.TextureLoader#)))
         (promise-main (get-load-texture-promise :path path
                                                 :loader loader))
         (promise-alpha (get-load-texture-promise :path alpha-path
                                                  :loader loader))
         (tex (make-texture-2d :name name
                               :path-list (if alpha-path
                                              (list path alpha-path)
                                              (list path))
                               :material nil
                               :rect-uv (make-rect-2d :x x
                                                      :y y
                                                      :width width
                                                      :height height))))
    (setf (gethash name *texture-promise-table*)
          (frame-promise-all
           (list promise-main promise-alpha)
           (lambda (values)
             (let ((image-bitmap (nth 0 values))
                   (alpha-bitmap (nth 1 values)))
               (setf (texture-2d-material tex)
                     (new (#j.THREE.MeshBasicMaterial#
                           (create map image-bitmap
                                   alpha-map alpha-bitmap
                                   transparent (if alpha-bitmap true false)
                                   color #xffffff)))))
             tex)
           :timeout-frame *load-texture-timeout-frames*))))

;; Note: not tested function
(defun.ps+ unload-texture (name)
  (let ((tex-promise (find-texture-promise name)))
    (unless tex-promise
      (error "The texture \"~A\" is not loaded." name))
    (frame-promise-then
     tex-promise
     (lambda (tex)
       (setf *texture-promise-table* (remove tex-promise *texture-promise-table*))
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
                                     path ref-count))))))))))

(defun.ps+ get-texture-promise (name)
  (let ((tex-promise (find-texture-promise name)))
    (unless tex-promise
      (error "The texture \"~A\" is not loaded." name))
    tex-promise))

;; --- utils --- ;;

(defun.ps get-texture-2d-width (texture-2d)
  (with-slots (material rect-uv) texture-2d
    (* material.map.image.width
       (rect-2d-width rect-uv))))

(defun.ps get-texture-2d-height (texture-2d)
  (with-slots (material rect-uv) texture-2d
    (* material.map.image.height
       (rect-2d-height rect-uv))))
