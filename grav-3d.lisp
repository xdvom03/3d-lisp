(defun start-program ()
  (defparameter *particles* nil)
  (defparameter *G* 100)
  (defparameter *step* 0.04)
  (add-particle '(0 0 500) '(0 0 0) 1000 1000)
  (add-particle '(100 0 500) '(0 30 0) 1 1)
  (add-particle '(0 200 500) '(0 0 10) 1 1))

(defun add-particle (position speed mass charge)
  (push (list position speed mass charge) *particles*))

(defun move-particles ()
  (let ((new-particles nil))
    (dolist (p *particles*)
      (let ((net-Fx 0)
            (net-Fy 0)
            (net-Fz 0))
        (dolist (p2 *particles*)
          (if (not (equal p p2))
              (let* ((dst (sqrt (reduce #'+
                                        (mapcar #'(lambda (a) (* a a))
                                                (mapcar #'-
                                                        (first p)
                                                        (first p2))))))
                     (force (/ (* (fourth p) (fourth p2) *G* -1)
                               (* dst dst)))
                     (dst-x (- (first (first p)) (first (first p2))))
                     (dst-y (- (second (first p)) (second (first p2))))
                     (dst-z (- (third (first p)) (third (first p2))))
                     (force-x (* force (/ dst-x dst)))
                     (force-y (* force (/ dst-y dst)))
                     (force-z (* force (/ dst-z dst))))
                (incf net-Fx force-x)
                (incf net-Fy force-y)
                (incf net-Fz force-z))))
        (push (list (mapcar #'(lambda (pos spd) (+ pos (* spd *step*)))
                            (first p)
                            (second p))
                    (mapcar #'(lambda (spd acc) (+ spd (* acc *step*)))
                            (second p)
                            (list (/ net-Fx (third p))
                                  (/ net-Fy (third p))
                                  (/ net-Fz (third p))))
                    (third p)
                    (fourth p))
              new-particles)))
    (setf *particles* (reverse new-particles))))

(defun create-objects (time)
  (move-particles)
  (setf *lines* nil)
  (dotimes (i (length *particles*))
    (let* ((p-new (nth i *particles*))
           (pos-new (first p-new)))
      (add-cube (mapcar #'+ pos-new (list 10 10 10)) 20 "#000"))))
