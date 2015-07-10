;;; Matrix
;;; ======

(defstruct matrix
  (items))

(defun matrix-create (size)
  (let ((matrix (make-array size))
	(bounds (sub1 size)))
    (loop for x from 0 to bounds
	  do (push (make-array size) matrix))
    (make-matrix :items matrix)))

(defun matrix-get (m x y)
  (elt (elt (matrix-items m) x) y))

(defun matrix-set (m x y v)
  (setf (elt (elt (matrix-items m) x) y) v))

(defun matrix-copy (m1 m2 count)
  (iterate #'(lambda (x y)
	       (matrix-set m2
			   x y
			   (matrix-get m1 x y)))
	   count))

(defun matrix-translate (x count)
  (if (= x count)
      0
    (if (= x -1)
	(sub1 count)
      x)))

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
