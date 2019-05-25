;;TBD: General cleanup, check what can be delegated to 3D-util.lisp
;;TBD: Adapt mouse control for Linux
;;TBD: Make init simpler to use - libraries :(
;;POSSIBLE BUG: Lots of rotation in one direction causes everything to behave oddly (investigate)


(defun init (&optional (compile-p t))
  ;;Constants
  (defparameter *triangles* nil) ;All triangles to be redrawn each tick
  (defparameter *lines* nil) ;All lines to be redrawn each tick
  (if compile-p
      (progn
        (setf *wish-pathname* "/usr/bin/wish")
        (in-package :ltk)
        (defparameter *view-center* (list 0 0 0)) ;Position of camera in space
        (defparameter *canvas-center* '(450 450)) ;Middle of the actual canvas
        (defparameter *screen-size* 900) ;Canvas dimensions
        (defparameter *fog* 0.5) ;Start of fog (division by zero protection)
        (defparameter *fov* 1) ;FOV in radians
        (defparameter *move-step* 30) ;How fast the camera moves
        (defparameter *rot-step* 0.02) ;How fast the camera rotates - radians per button press
        (defparameter *x-dir* (list 1 0 0))
        (defparameter *y-dir* (list 0 1 0))
        (defparameter *z-dir* (list 0 0 1))
        (defparameter *norm* (/ *screen-size* (* 2 (tan (/ *fov* 2))))) ;Distance of projective plane
        (defparameter *mouse-fix* nil) ;Where the mouse should return to, set by clicking, TEMPORARY
  )))


;;;-----------------------------------------------------------------------------------------------------------------;;;


;;GLOBAL FUNCTIONS - CHANGING THE ENVIRONMENT


(defun add-line (start end color)
  ;;Adds a line to the global list
  (push (list start end color) *lines*))

(defun add-cube (bottom-left-front size color)
  ;;Adds the lines forming a cube (with diagonals) to the global list
  (let ((lbf (mapcar #'+ bottom-left-front (list 0 0 0)))
        (lbb (mapcar #'+ bottom-left-front (list 0 0 size)))
        (ltf (mapcar #'+ bottom-left-front (list 0 size 0)))
        (ltb (mapcar #'+ bottom-left-front (list 0 size size)))
        (rbf (mapcar #'+ bottom-left-front (list size 0 0)))
        (rbb (mapcar #'+ bottom-left-front (list size 0 size)))
        (rtf (mapcar #'+ bottom-left-front (list size size 0)))
        (rtb (mapcar #'+ bottom-left-front (list size size size))))
    (add-line lbf rbf color)
    (add-line ltf rtf color)
    (add-line ltf lbf color)
    (add-line rtf rbf color)
    
    (add-line lbb rbb color)
    (add-line ltb rtb color)
    (add-line ltb lbb color)
    (add-line rtb rbb color)

    (add-line lbf lbb color)
    (add-line ltf ltb color)
    (add-line rtf rtb color)
    (add-line rbf rbb color)))

(defun add-triangle (p1 p2 p3 color outline-colors)
  ;;If color is nil, the outline isn't printed
  ;;Colors in order p1p2, p2p3, p3p1
  (labels ((outline-part (point-a point-b color)
             (if color
                 (add-line point-a point-b color))))
    (push (list p1 p2 p3 color) *triangles*)
    (outline-part p1 p2 (first outline-colors))
    (outline-part p2 p3 (second outline-colors))
    (outline-part p3 p1 (third outline-colors)))
  (push (list p1 p2 p3 color) *triangles*)
  (if (first outline-colors)
      (add-line p1 p2 (first outline-colors))))


;;;-----------------------------------------------------------------------------------------------------------------;;;


;;RENDERING UTILITIES & RENDER


(defun render (c)
  (dolist (i *triangles*)
    (apply #'draw-triangle (append (list c) (rotate-coords (remove-last i)) (last i))))
  (dolist (i *lines*)
    (apply #'draw-line (append (list c) (rotate-coords (remove-last i)) (last i)))))

(defun rotate-coords (coords)
  ;;Accepts list of points and returns their rotated new locations
  (mapcar #'(lambda (point) (rotate-around-point point (mapcar #'- *view-center*) *x-dir* *y-dir* *z-dir*)) coords))

(defun draw-line (c p1 p2 color)
  ;;Draws a line segment given two points
  (if (not (and (in-fog-p p1 *fog*) (in-fog-p p2 *fog*)))
      (itemconfigure c (create-line c (real-locs p1 p2)) :fill color)))


;;;-----------------------------------------------------------------------------------------------------------------;;;


;;TRIANGLE FOG AND RENDERING

(defun sort-by-fog (points)
  (let ((in nil)
        (out nil))
    (dolist (point points (list in out))
      (if (in-fog-p point *fog*)
          (push point in)
          (push point out)))))

(defun draw-triangle (c p1 p2 p3 color)
  (let ((points-in-fog (count t (mapcar #'(lambda (a) (in-fog-p a *fog*))
                                        (list p1 p2 p3)))))
    (case points-in-fog
      (3 nil) ;May be unnecessary
      (2 (let* ((sorted (sort-by-fog (list p1 p2 p3)))
                (in (first sorted))
                (out (second sorted))
                (fog-borders (list (fog-cutoff-loc (first in) (first out))
                                   (fog-cutoff-loc (second in) (first out)))))
           (itemconfigure c (create-polygon c (append (apply #'real-loc (first out)) fog-borders)) :fill color)))
      (1 (let* ((sorted (sort-by-fog (list p1 p2 p3)))
                (in (first sorted))
                (out (second sorted))
                (fog-borders (list (fog-cutoff-loc (first in) (first out))
                                   (fog-cutoff-loc (first in) (second out)))))
           (itemconfigure c (create-polygon c (append (apply #'real-loc (second out)) (apply #'real-loc (first out)) fog-borders)) :fill color)))
      (0 (itemconfigure c (create-polygon c (list (apply #'real-loc p1) (apply #'real-loc p2) (apply #'real-loc p3))) :fill color)))))


;;;-----------------------------------------------------------------------------------------------------------------;;;


;;RUNNING, UPDATING, CREATING WIDGETS


(defun create-objects (time)
  (identity time)
  (init nil)
  (add-cube '(-500 -500 -500) 100 "#000")
  (add-cube '(-500 -500 500) 100 "#001")
  (add-cube '(-500 500 -500) 100 "#012")
  (add-cube '(-500 500 500) 100 "#123")
  (add-cube '(500 -500 -500) 100 "#234")
  (add-cube '(500 -500 500) 100 "#345")
  (add-cube '(500 500 -500) 100 "#456")
  (add-cube '(500 500 500) 100 "#567")
  (add-cube '(-500 -500 0) 100 "#678")
  (add-cube '(-500 500 0) 100 "#789")
  (add-cube '(500 -500 0) 100 "#89A")
  (add-cube '(500 500 0) 100 "#9AB")
  (add-cube '(0 -500 0) 100 "#ABC")
  (add-cube '(0 500 0) 100 "#BCD")
  (add-cube '(0 0 0) 100 "#CDE")
  (add-cube '(500 0 500) 100 "#DEF")
  (add-cube '(-500 0 500) 100 "#BAF")
  (add-cube '(500 0 -500) 100 "#312")
  (add-cube '(-500 0 -500) 100 "#156")
  (add-cube '(0 0 500) 100 "#495")
  (add-cube '(0 0 -500) 100 "#624")
  (add-cube '(0 500 500) 100 "#981")
  (add-cube '(0 500 -500) 100 "#3A4")
  (add-cube '(0 -500 500) 100 "#C01")
  (add-cube '(0 -500 -500) 100 "#BFF")
  (add-cube '(500 0 0) 100 "#CA1")
  (add-cube '(-500 0 0) 100 "#BAC")
  (add-triangle '(500 500 700) '(0 500 700) '(200 200 700) "#DEB" (list "#BDE" nil "#196")))

(defun tick-compute (num widgets)
  ;;Creates new widgets and calls main functions
  (let ((c (make-instance 'canvas
                          :width *screen-size*
                          :height *screen-size*))
        (b (make-instance 'button
                          :text num)))
    ;;Binding forces a parameter I don't want, hence the identities
    (bind c 
          "<Left>" 
          #'(lambda (a) (identity a) (setf *view-center* (mapcar #'+ *view-center* (mapcar #'(lambda (a) (* a *move-step*)) *x-dir*)))))
    (bind c 
          "<Right>" 
          #'(lambda (a) (identity a) (setf *view-center* (mapcar #'+ *view-center* (mapcar #'(lambda (a) (* a *move-step*)) (mapcar #'- *x-dir*))))))
    (bind c 
          "<Up>" 
          #'(lambda (a) (identity a) (setf *view-center* (mapcar #'+ *view-center* (mapcar #'(lambda (a) (* a *move-step*)) *y-dir*)))))
    (bind c 
          "<Down>" 
          #'(lambda (a) (identity a) (setf *view-center* (mapcar #'+ *view-center* (mapcar #'(lambda (a) (* a *move-step*)) (mapcar #'- *y-dir*))))))
    (bind c 
          "<q>" 
          #'(lambda (a) (identity a) (setf *view-center* (mapcar #'+ *view-center* (mapcar #'(lambda (a) (* a *move-step*)) *z-dir*)))))
    (bind c 
          "<a>" 
          #'(lambda (a) (identity a) (setf *view-center* (mapcar #'+ *view-center* (mapcar #'(lambda (a) (* a *move-step*)) (mapcar #'- *z-dir*))))))
    (bind c
          "<w>"
          #'(lambda (a) (identity a) (rot-cam-x *rot-step*)))
    (bind c
          "<s>"
          #'(lambda (a) (identity a) (rot-cam-x (- *rot-step*))))
    (bind c
          "<e>"
          #'(lambda (a) (identity a) (rot-cam-y *rot-step*)))
    (bind c
          "<d>"
          #'(lambda (a) (identity a) (rot-cam-y (- *rot-step*))))
    (bind c
          "<r>"
          #'(lambda (a) (identity a) (rot-cam-z *rot-step*)))
    (bind c
          "<f>"
          #'(lambda (a) (identity a) (rot-cam-z (- *rot-step*))))
    (focus c)
    (create-objects num)
    (render c)
    (pack c)
    (pack b)
    (dolist (i widgets)
      (destroy i))
    (list c b)))

(defun tick (a widgets)
  ;;Main recursive function
  ;;Keeps updating
  (let ((new (tick-compute a widgets)))
    (after 0 #'(lambda () (tick (1+ a) new)))))

(defun run ()
  (init t)
  (with-ltk ()
    (tick 0 nil)))
