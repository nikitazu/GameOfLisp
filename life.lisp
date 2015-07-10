(ql:quickload :lispbuilder-sdl)

;;; Settigns
;;; ========

(defvar *pixel-size* nil)
(defvar *cells-count* nil)
(defvar *cell-size* nil)
(defvar *matrix* nil)
(defvar *old-matrix* nil)
(defvar *dead-color* nil)
(defvar *alive-color* nil)

;;; Initialization
;;; ==============

(defun main ()
  (init-settings)

  (sdl:with-init
   ()
   (sdl:window *pixel-size* *pixel-size*)
   (setf (sdl:frame-rate) 60)
   (sdl:with-events
    ()
    (:quit-event () t)
    (:key-down-event () (sdl:push-quit-event))
    (:idle
     ()
     (update-old-matrix)
     (game-step)
     (sdl:update-display)))))

(defun init-settings ()
  (setf *pixel-size* 1000)
  (setf *cells-count* 150)
  (setf *cell-size* (/ *pixel-size* *cells-count*))
  (setf *matrix* (matrix-create *cells-count*))
  (setf *old-matrix* (matrix-create *cells-count*))
  (iterate #'(lambda (x y)
	       (matrix-set *matrix* x y (= (random 10) 0)))
	   *cells-count*)
  (setf *dead-color* (sdl:color :r 0 :g 0 :b 0))
  (setf *alive-color* (sdl:color :r 200 :g 0 :b 200)))


;;; Game of Life
;;; ============

(defun game-step ()
  (iterate #'(lambda (x y)
	       (let ((count (game-count-alive x y))
		     (old-state (matrix-get *old-matrix* x y)))
		 (unless (or (and old-state
				  (or (= count 2) (= count 3)))
			     (and (not old-state)
				  (not (= count 3))))
		   (let ((new-state (not old-state)))
		     (matrix-set *matrix* x y new-state)
		     (draw-cell x y new-state)))))
	   *cells-count*))

(defun game-count-alive (x1 y1)
  (let ((x0 (matrix-translate (sub1 x1)))
	(x2 (matrix-translate (add1 x1)))
	(y0 (matrix-translate (sub1 y1)))
	(y2 (matrix-translate (add1 y1))))
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

(defun update-old-matrix ()
  (iterate #'(lambda (x y)
	       (matrix-set *old-matrix*
			   x y
			   (matrix-get *matrix* x y)))
	   *cells-count*))

;;; Draw
;;; ====

(defun draw-cell (x y alive-p)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* (* *cell-size* x)
					       (* *cell-size* y)
					       *cell-size*
					       *cell-size*)
		:color (if alive-p
			   *alive-color*
			 *dead-color*)))


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

(main)
