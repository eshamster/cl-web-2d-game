(in-package :cl-user)
(defpackage cl-web-2d-game/graphics/2d-geometry
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/graphics/font
        :cl-web-2d-game/graphics/texture)
  (:export :make-line
           :make-lines
           :make-solid-rect
           :make-wired-rect
           :make-solid-regular-polygon
           :make-wired-regular-polygon
           :make-solid-circle
           :make-wired-circle
           :make-wired-polygon
           :make-solid-polygon
           :make-texture-model
           :make-texture-model-promise
           :make-text-model-promise
           :change-model-color
           :change-geometry-uvs

           :get-mesh-width
           :get-mesh-height
           :get-mesh-size))
(in-package :cl-web-2d-game/graphics/2d-geometry)

(enable-ps-experiment-syntax)

;; --- basic funcations and macros

(defun.ps push-vertices-to (geometry raw-vertex-lst)
  (dolist (vertex-as-lst raw-vertex-lst)
    (geometry.vertices.push
     (new (#j.THREE.Vector3# (nth 0 vertex-as-lst)
                             (nth 1 vertex-as-lst)
                             0)))))

(defun.ps push-faces-to (geometry raw-face-lst)
  (dolist (face-as-lst raw-face-lst)
    (geometry.faces.push
     (new (#j.THREE.Face3# (nth 0 face-as-lst)
                           (nth 1 face-as-lst)
                           (nth 2 face-as-lst))))))

(defun.ps to-rad (degree)
  (/ (* degree pi) 180))

(defun.ps make-line-model (geometry color)
  (let ((material (new (#j.THREE.LineBasicMaterial# (create :color color)))))
    (new (#j.THREE.Line# geometry material))))

(defmacro.ps+ def-wired-geometry (name args &body body)
  (with-ps-gensyms (geometry)
    `(defun.ps ,name (&key ,@args color)
       (let ((,geometry (new (#j.THREE.Geometry#))))
         (flet ((push-vertices (&rest rest)
                  (push-vertices-to ,geometry rest)))
           ,@body)
         (make-line-model ,geometry color)))))

(defun.ps make-solid-model (geometry color)
  (let ((material (new (#j.THREE.MeshBasicMaterial# (create :color color)))))
    (new (#j.THREE.Mesh# geometry material))))

(defmacro.ps+ def-solid-geometry (name args &body body)
  (with-ps-gensyms (geometry)
    `(defun.ps ,name (&key ,@args color)
       (let ((,geometry (new (#j.THREE.Geometry#))))
         (flet ((push-vertices (&rest rest)
                  (push-vertices-to ,geometry rest))
                (push-faces (&rest rest)
                  (push-faces-to ,geometry rest)))
           ,@body)
         (make-solid-model ,geometry color)))))

;; --- utils --- ;;

(defun.ps get-mesh-width (mesh)
  (- mesh.geometry.bounding-box.max.x
     mesh.geometry.bounding-box.min.x))

(defun.ps get-mesh-height (mesh)
  (- mesh.geometry.bounding-box.max.y
     mesh.geometry.bounding-box.min.y))

(defun.ps+ get-mesh-size (mesh)
  (list :width (get-mesh-width mesh)
        :height (get-mesh-height mesh)))

;; --- line --- ;;

(def-wired-geometry make-line (pos-a pos-b)
  (push-vertices (list (aref pos-a 0) (aref pos-a 1))
                 (list (aref pos-b 0) (aref pos-b 1))))

(def-wired-geometry make-lines (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices (list (aref pnt 0) (aref pnt 1)))))

;; --- rectangle --- ;;

(def-solid-geometry make-solid-rect (width height)
  (push-vertices (list 0 0) (list width 0)
                 (list width height) (list 0 height))
  (push-faces '(0 1 2) '(2 3 0)))

(def-wired-geometry make-wired-rect (width height)
  (push-vertices (list 0 0) (list width 0)
                 (list width height) (list 0 height)
                 (list 0 0)))

;; --- textured model --- ;;

(defun.ps make-rect-uvs (x y width height)
  (flet ((new-uv (u v)
           (new (#j.THREE.Vector2# u v))))
    (list (list (new-uv x y)
                (new-uv (+ x width) y)
                (new-uv (+ x width) (+ y height)))
          (list (new-uv (+ x width) (+ y height))
                (new-uv x (+ y height))
                (new-uv x y)))))

(defun.ps change-geometry-uvs (texture geometry x y width height)
  (check-type texture texture-2d)
  (with-slots ((base-x x) (base-y y)
               (base-width width) (base-height height)) (texture-2d-rect-uv texture)
    (let ((uvs (@ geometry face-vertex-uvs 0))
          (count-outer 0))
      (dolist (uv (make-rect-uvs (+ base-x (* base-width x))
                                 (+ base-y (* base-height y))
                                 (* base-width width)
                                 (* base-height height)))
        (if (>= count-outer uvs.length)
            (uvs.push uv)
            (let ((count-inner 0))
              (dolist (vector uv)
                (setf (@ (nth count-inner (nth count-outer uvs)) x) vector.x
                      (@ (nth count-inner (nth count-outer uvs)) y) vector.y)
                (incf count-inner))))
        (incf count-outer)))
    (setf geometry.uvs-need-update t)))

(defun.ps+ calc-texture-size (width height size-type texture)
  (let ((texture-width (get-texture-2d-width texture))
        (texture-height (get-texture-2d-height texture)))
    (flet ((calc-scale (target base-len)
             (ecase size-type
               (:absolute (when target (/ target base-len)))
               (:relative target))))
      (let ((width-scale (calc-scale width texture-width))
            (height-scale (calc-scale height texture-height)))
        (cond ((and (not width-scale) (not height-scale))
               (setf width-scale 1 height-scale 1))
              ((not width-scale)
               (setf width-scale height-scale))
              ((not height-scale)
               (setf height-scale width-scale)))
        (assert (and width-scale height-scale))
        (list (* texture-width width-scale)
              (* texture-height height-scale))))))

(defun.ps make-texture-model (&key width height (size-type :absolute) texture)
  (check-type texture texture-2d)
  (let* ((geometry (new (#j.THREE.Geometry#)))
         (texture-size (calc-texture-size width height size-type texture))
         (w (car texture-size))
         (h (cadr texture-size)))
    (push-vertices-to geometry
                      (list (list 0 0) (list w 0)
                            (list w h) (list 0 h)))
    (push-faces-to geometry (list '(0 1 2) '(2 3 0)))
    (change-geometry-uvs texture geometry 0 0 1 1)
    (geometry.compute-face-normals)
    (geometry.compute-vertex-normals)
    (new (#j.THREE.Mesh# geometry
                         (texture-2d-material texture)))))

(defun.ps+ make-texture-model-promise (&key width height (size-type :absolute) texture-name)
  "Return a frame-promise that passes mesh that has specified texture as its material.
If size-type is :absolute, the passed width and height are used as-is.
If size-type is :relative, the passed width and height means how scale up the texture.
If only either width or height is passed, the image keeps its aspect ratio.
If neither width nor height is passed, the image keeps its size
 (This is equals to (:width 1 :height 1 :size-type :relative)."
  (frame-promise-then
   (get-texture-promise texture-name)
   (lambda (texture)
     (make-texture-model :width width :height height
                         :size-type size-type
                         :texture texture))))

;; --- regular polygon --- ;;

(def-solid-geometry make-solid-regular-polygon (r n (start-angle 0))
  (dotimes (i n)
    (let ((angle (to-rad (+ (/ (* 360 i) n) start-angle))))
      (push-vertices (list (* r (cos angle))
                           (* r (sin angle))))))
  (push-vertices (list 0 0))
  (dotimes (i n)
    (push-faces (list n i (rem (1+ i) n)))))

(def-wired-geometry make-wired-regular-polygon (r n (start-angle 0))
  (dotimes (i (1+ n))
    (let ((angle (to-rad (+ (/ (* 360 i) n) start-angle))))
      (push-vertices (list (* r (cos angle))
                           (* r (sin angle)))))))

;; --- circle --- ;;
;; TODO: Adaptively decide the 'n' according to the 'r'

(defun.ps+ make-solid-circle (&key r color)
  (make-solid-regular-polygon :r r :n 60 :color color))

(defun.ps+ make-wired-circle (&key r color)
  (make-wired-regular-polygon :r r :n 60 :color color))

;; --- arbitrary polygon --- ;;

(def-wired-geometry make-wired-polygon (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices pnt))
  (push-vertices (nth 0 pnt-list)))

(def-solid-geometry make-solid-polygon (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices pnt))
  (let ((len (length pnt-list)))
    (dotimes (i (1- len))
      (push-faces (list 0
                        (+ i 1)
                        (rem (+ i 2) len))))))

;; --- auxiliary functions --- ;;

(defun.ps change-model-color (model-2d new-color-rgb)
  (with-slots (model) model-2d
    (setf model.material.color (new (#j.THREE.Color# new-color-rgb)))
    (setf model.material.needs-update t))
  model-2d)

;; --- text --- ;;

(defun.ps make-text-model-promise (text &key size color
                                        (curve-segments 3) (font-name "helvetiker"))
  (frame-promise-then
   (get-font-promise font-name "regular")
   (lambda (font)
     (let ((geometry (new (#j.THREE.TextGeometry#
                           text
                           (create "size" size "height" 0
                                   "curveSegments" curve-segments "font" font))))
           (material (new (#j.THREE.MeshBasicMaterial# (create "color" color)))))
       (geometry.compute-bounding-box)
       (geometry.compute-vertex-normals)
       (geometry.compute-face-normals)
       (let ((mesh (new (#j.THREE.Mesh# geometry material))))
         mesh)))))
