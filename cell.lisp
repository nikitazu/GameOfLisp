;;; Cell
;;; ====

(defstruct cell
  (state))

(defun cell-is-stable (old-state count)
  (or (and old-state
	   (or (= count 2) (= count 3)))
      (and (not old-state)
	   (not (= count 3)))))

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
  (if (matrix-get m x y)
      1
    0))
