;;; Cell
;;; ====

(defstruct cell
  (state))

(defun cell-is-stable (old-state count)
  (or (and old-state
	   (or (= count 2) (= count 3)))
      (and (not old-state)
	   (not (= count 3)))))

(defun cell-count-alive (x1 y1 count)
  (let ((x0 (matrix-translate (sub1 x1) count))
	(x2 (matrix-translate (add1 x1) count))
	(y0 (matrix-translate (sub1 y1) count))
	(y2 (matrix-translate (add1 y1) count)))
    (+ (matrix-get-alive-as-number x0 y0)
       (matrix-get-alive-as-number x0 y1)
       (matrix-get-alive-as-number x0 y2)
       (matrix-get-alive-as-number x2 y0)
       (matrix-get-alive-as-number x2 y1)
       (matrix-get-alive-as-number x2 y2)
       (matrix-get-alive-as-number x1 y0)
       (matrix-get-alive-as-number x1 y2))))

(defun matrix-get-alive-as-number (x y)
  (if (matrix-get *old-matrix* x y)
      1
    0))
