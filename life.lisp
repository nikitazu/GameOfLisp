(ql:quickload :lispbuilder-sdl)

(load "matrix.lisp")
(load "cell.lisp")

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
     (matrix-copy *matrix* *old-matrix*)
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
	       (let ((count (cell-count-alive *old-matrix* x y))
		     (old-state (matrix-get *old-matrix* x y)))
		 (unless (cell-is-stable old-state count)
		   (let ((new-state (not old-state)))
		     (matrix-set *matrix* x y new-state)
		     (draw-cell x y new-state)))))
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

(main)
