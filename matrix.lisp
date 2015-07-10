;;; Matrix
;;; ======

(defstruct matrix
  items
  size)

(defun matrix-create (size)
  (let ((matrix (make-array size))
	(bounds (sub1 size)))
    (loop for x from 0 to bounds
	  do (push (make-array size) matrix))
    (make-matrix :items matrix
		 :size size)))

(defun matrix-get (m x y)
  (elt (elt (matrix-items m) x) y))

(defun matrix-set (m x y v)
  (setf (elt (elt (matrix-items m) x) y) v))

(defun matrix-copy (m1 m2 f)
  (iterate #'(lambda (x y)
	       (matrix-set m2
			   x y
			   (funcall f (matrix-get m1 x y))))
	   (matrix-size m1)))

(defun matrix-translate (m x)
  (let ((s (matrix-size m)))
    (if (= x s)
	0
      (if (= x -1)
	  (sub1 s)
	x))))

(defun matrix-iterate (m f)
  (iterate #'(lambda (x y)
	       (funcall f x y (matrix-get m x y)))
	   (matrix-size m)))

;;; Utils
;;; =====

(defun iterate (f size)
  (let ((bounds (sub1 size)))
    (loop for x from 0 to bounds
	  do (loop for y from 0 to bounds
		   do (funcall f x y)))))

(defun sub1 (n)
  (- n 1))

(defun add1 (n)
  (+ n 1))
