;;; Matrix
;;; ======

(defun matrix-create (size)
  (let ((matrix (make-array size))
	(bounds (sub1 size)))
    (loop for x from 0 to bounds
	  do (push (make-array size) matrix))
    matrix))

(defun iterate (f size)
  (let ((bounds (sub1 size)))
    (loop for x from 0 to bounds
	  do (loop for y from 0 to bounds
		   do (funcall f x y)))))

(defun matrix-get (m x y)
  (elt (elt m x) y))

(defun matrix-set (m x y v)
  (setf (elt (elt m x) y) v))

(defun matrix-translate (x)
  (if (= x *cells-count*)
      0
    (if (= x -1)
	(sub1 *cells-count*)
      x)))

;;; Utils
;;; =====

(defun sub1 (n)
  (- n 1))

(defun add1 (n)
  (+ n 1))
