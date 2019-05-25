(defun sq (a)
  (* a a))

(defun transpose (mat)
  ;;Returns a transposed matrix
  ;;Requires a list of lists
  (apply #'mapcar #'list mat))

(defun mv-multiply (mat vec)
  ;;Matrix-vector multiplication for square matrices
  ;;Behaviour unspecified for non-matching sizes
  (reduce #'(lambda (a b) (mapcar #'+ a b))
          (mapcar #'(lambda (num vec) (mapcar #'(lambda (element) (* num element)) vec))
                  vec
                  mat)))

(defun pythagoras (list)
  ;;Any number of dimensions
  (sqrt (reduce #'+ (mapcar #'sq list))))

(defun distance (point1 point2)
  ;;Any number of dimensions
  (pythagoras (mapcar #'- point1 point2)))

(defun perpendicular-base (point line-point)
  ;;s for slope
  ;;foot of perpendicular line to [0; line-point] going through point
  (let* ((sx (first line-point))
         (sy (second line-point))
         (sz (third line-point))
         (px (first point))
         (py (second point))
         (pz (third point))
         (a (/ (+ (* sx px) (* sy py) (* sz pz)) (+ (sq sx) (sq sy) (sq sz)))))
    (list (* sx a) (* sy a) (* sz a))))

(defun vector-divide (v1 v2)
  ;;The two vectors must have the same direction, else behavior is unspecified
  ;;Returns a scalar
  (let ((x1 (first v1))
        (x2 (first v2))
        (y1 (second v1))
        (y2 (second v2))
        (z1 (third v1))
        (z2 (third v2)))
    (if (or (zerop x1) (zerop x2))
        (if (or (zerop y1) (zerop y2))
            (if (or (zerop z1) (zerop z2))
                0
                (/ z1 z2))
            (/ y1 y2))
        (/ x1 x2))))

(defun cross-product (v1 v2)
  ;;Returns the cross product of vector 1 and vector 2
  ;;3D dependent
  (let ((x1 (first v1))
        (x2 (first v2))
        (y1 (second v1))
        (y2 (second v2))
        (z1 (third v1))
        (z2 (third v2)))
    (list (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

(defun turn (point line-point angle)
  ;;Clockwise when looking from the origin towards line-point
  ;;Returns destination of point when turned "angle" radians around the line given by the origin and "line-point"
  ;;C is the center of the circle on which the point rotates
  (let ((C (perpendicular-base point line-point)))
    (if (equal C point)
        point
        (let* ((CO (mapcar #'- (list 0 0 0) (if (= (distance C (list 0 0 0)) 0)
                                                line-point
                                                C)))
               (CP (mapcar #'- point C))
               (up (cross-product CO CP))
               (up-scaled (mapcar #'(lambda (a) (* a (/ (pythagoras CP) (pythagoras up))))
                                  up))
               (one-way (mapcar #'+
                                (if (equal C (list 0 0 0))
                                    (list 0 0 0)
                                    C)
                                (mapcar #'(lambda (a) (* a (sin angle))) up-scaled)
                                (mapcar #'(lambda (a) (* a (cos angle))) CP)))
               (other-way (mapcar #'+
                                  (if (equal C (list 0 0 0))
                                      (list 0 0 0)
                                      C)
                                  (mapcar #'(lambda (a) (* a (sin (- angle)))) up-scaled)
                                  (mapcar #'(lambda (a) (* a (cos angle))) CP)))
               (middle (mapcar #'(lambda (a) (/ a 2))
                               (mapcar #'+ one-way other-way)))
               (first-signed (vector-divide (mapcar #'- middle one-way) (cross-product (mapcar #'- point C) line-point))))
          (if (or (equal one-way other-way)
                  (equal (> first-signed 0)
                         (> (mod angle 6.283185307179) 3.14159265358979)))
              one-way
              other-way)))))

(defun rotated-loc (point x-dir y-dir z-dir)
  (let* ((cam-dir (list x-dir y-dir z-dir))
         (inverse-cam (transpose cam-dir)))
    (mv-multiply inverse-cam point)))

(defun rotate-around-point (point center x-dir y-dir z-dir)
  (mapcar #'+ center (rotated-loc (mapcar #'- point center) x-dir y-dir z-dir)))
