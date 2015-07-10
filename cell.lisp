;;; Cell
;;; ====

(defstruct cell
  state
  count)

(defun cell-create-random ()
  (make-cell :state (= (random 10) 0)
	     :count 0))

(defun cell-is-stable (old-state count)
  (or (and old-state
	   (or (= count 2) (= count 3)))
      (and (not old-state)
	   (not (= count 3)))))

(defun cell-near-update (m x1 y1 f)
  (let ((x0 (matrix-translate m (sub1 x1)))
	(x2 (matrix-translate m (add1 x1)))
	(y0 (matrix-translate m (sub1 y1)))
	(y2 (matrix-translate m (add1 y1))))
    (matrix-update-count m x0 y0 f)
    (matrix-update-count m x0 y1 f)
    (matrix-update-count m x0 y2 f)
    (matrix-update-count m x2 y0 f)
    (matrix-update-count m x2 y1 f)
    (matrix-update-count m x2 y2 f)
    (matrix-update-count m x1 y0 f)
    (matrix-update-count m x1 y2 f)))

(defun matrix-update-state (m x y state)
  (let ((c (matrix-get m x y)))
    (setf (cell-state c)
	  state)))

(defun matrix-update-count (m x y f)
  (let ((c (matrix-get m x y)))
    (setf (cell-count c)
	  (funcall f (cell-count c)))))

(defun cell-count-alive (m x1 y1)
  (let ((x0 (matrix-translate m (sub1 x1)))
	(x2 (matrix-translate m (add1 x1)))
	(y0 (matrix-translate m (sub1 y1)))
	(y2 (matrix-translate m (add1 y1))))
    (+ (matrix-get-alive-as-number m x0 y0)
       (matrix-get-alive-as-number m x0 y1)
       (matrix-get-alive-as-number m x0 y2)
       (matrix-get-alive-as-number m x2 y0)
       (matrix-get-alive-as-number m x2 y1)
       (matrix-get-alive-as-number m x2 y2)
       (matrix-get-alive-as-number m x1 y0)
       (matrix-get-alive-as-number m x1 y2))))

(defun matrix-get-alive-as-number (m x y)
  (if (cell-state (matrix-get m x y))
      1
    0))
