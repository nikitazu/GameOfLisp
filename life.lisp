(ql:quickload :lispbuilder-sdl)

;;; Settigns
;;; ========

(defvar *pixel-size* nil)
(defvar *cells-count* nil)
(defvar *cell-size* nil)
(defvar *matrix* nil)
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
     (game-step)
     (draw-matrix)
     (sdl:update-display)))))

(defun init-settings ()
  (setf *pixel-size* 600)
  (setf *cells-count* 200)
  (setf *cell-size* (/ *pixel-size* *cells-count*))
  (setf *matrix* (matrix-create *cells-count*))
  (matrix-iterate #'(lambda (x y)
		      (matrix-set x y (= (random 2) 0)))
		  *cells-count*)
  (setf *dead-color* (sdl:color :r 255 :g 0 :b 0))
  (setf *alive-color* (sdl:color :r 0 :g 0 :b 0)))


;;; Game of Life
;;; ============

(defun game-step ()
  (matrix-iterate #'(lambda (x y)
		      (let ((count (game-count-alive x y))
			    (alive (matrix-get x y)))
			(if alive
			    (if (or (= count 2) (= count 3))
				'survives
			      (matrix-set x y nil))
			  (if (= count 3)
			      (matrix-set x y t)
			    'left-empty))))
		  *cells-count*))

(defun game-count-alive (x0 y0)
  (let ((alive-count 0))
    (matrix-iterate-near #'(lambda (x y)
			     (when (matrix-get x y)
			       (setf alive-count (add1 alive-count))))
			 x0
			 y0)
    alive-count))

;;; Draw
;;; ====

(defun draw-matrix ()
  (matrix-iterate #'(lambda (x y)
		      (draw-cell x y (matrix-get x y)))
		  *cells-count*))

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

(defun matrix-iterate (f size)
  (let ((bounds (sub1 size)))
    (loop for x from 0 to bounds
	  do (loop for y from 0 to bounds
		   do (funcall f x y)))))

(defun matrix-iterate-near (f x y)
  (let ((x0 (sub1 x))
	(x2 (add1 x))
	(y0 (sub1 y))
	(y2 (add1 y)))
    (funcall f x0 y0)
    (funcall f x0 y)
    (funcall f x0 y2)
    (funcall f x2 y0)
    (funcall f x2 y)
    (funcall f x2 y2)
    (funcall f x y0)
    (funcall f x y2))
  nil)

(defun matrix-get (x y)
  (elt (elt *matrix*
	    (matrix-translate x))
       (matrix-translate y)))

(defun matrix-set (x y v)
  ;(format t "~a:~a=~a " x y v)
  (setf (elt (elt *matrix* x) y) v))

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
