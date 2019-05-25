(defun canvas-loc (x y z)
  ;;Input 3D relative to camera
  ;;Output 2D coords on canvas
  (let ((abs-xy (mapcar #'(lambda (a) (/ a (/ z *norm*))) (list x y))))
    (mapcar #'+ abs-xy *canvas-center*)))

(defun real-locs (point1 point2)
  ;;Accounts for fog
  ;;Input two points in 3D space
  ;;Output 2D coords on canvas
  ;;Breaks if both points are in fog
  (if (in-fog-p point1 *fog*)
      (append (fog-cutoff-loc point1 point2) (apply #'real-loc point2))
      (if (in-fog-p point2 *fog*)
          (append (apply #'real-loc point1)
                  (fog-cutoff-loc point2 point1))
          (append (apply #'real-loc point1)
                  (apply #'real-loc point2)))))

(defun real-loc (x y z)
  ;;Input 3D coords absolute (not adjusted for camera)
  ;;Output 2D canvas coords
  (let* ((rel-loc (mapcar #'+ (list x y z) *view-center*))
         (rel-x (first rel-loc))
         (rel-y (second rel-loc))
         (rel-z (third rel-loc))
         (abs-xy (mapcar #'(lambda (a) (/ a (/ rel-z *norm*))) (list rel-x rel-y))))
    (mapcar #'+ abs-xy *canvas-center*)))

(defun in-fog-p (point fog)
  ;;Returns T if the point is not in the visible range
  (< (+ (third point) (third *view-center*)) fog))

(defun fog-cutoff-loc (in out)
  ;;Returns the point where the line should be cut off by fog
  ;;Returns a point on the canvas
  ;;in means coords of the point in the fog, out the point outside the fog
  (let* ((rel-in (mapcar #'+ *view-center* in))
         (rel-out (mapcar #'+ *view-center* out))
         (outZ (- (third rel-out) *fog*))
         (inZ (- *fog* (third rel-in)))
         (dx (- (first rel-in) (first rel-out)))
         (dy (- (second rel-in) (second rel-out))))
    (canvas-loc (+ (first rel-out) (* dx (/ outZ (+ outZ inZ))))
                (+ (second rel-out) (* dy (/ outZ (+ outZ inZ))))
                *fog*)))

(defun remove-nth (n lst)
  ;;returns list without nth element (indexing from 0)
  (append (subseq lst 0 n) (nthcdr (1+ n) lst)))

(defun remove-last (lst)
  ;;returns list without last element
  (remove-nth (1- (length lst)) lst))









(defun rot-cam-x (angle)
  (let ((old-y *y-dir*)
        (old-z *z-dir*))
    (setf *y-dir* (turn old-y *x-dir* angle))
    (setf *z-dir* (turn old-z *x-dir* angle))))

(defun rot-cam-y (angle)
  (let ((old-x *x-dir*)
        (old-z *z-dir*))
    (setf *x-dir* (turn old-x *y-dir* angle))
    (setf *z-dir* (turn old-z *y-dir* angle))))

(defun rot-cam-z (angle)
  (let ((old-x *x-dir*)
        (old-y *y-dir*))
    (setf *x-dir* (turn old-x *z-dir* angle))
    (setf *y-dir* (turn old-y *z-dir* angle))))

(defun rot-cam-absolute (angle axis)
  ;;Rotates camera about a line not relative to current rotation
  (let ((old-x *x-dir*)
        (old-y *y-dir*)
        (old-z *z-dir*))
    (setf *x-dir* (turn old-x axis angle))
    (setf *y-dir* (turn old-y axis angle))
    (setf *z-dir* (turn old-z axis angle))))
