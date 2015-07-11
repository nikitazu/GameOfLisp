(ql:quickload :lispbuilder-sdl)
(ql:quickload :cl-fad)

(load "matrix.lisp")
(load "cell.lisp")

;;; Settigns
;;; ========

(defvar *pixel-size* nil)
(defvar *cells-count* nil)
(defvar *randomness* nil)
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
     (matrix-copy *matrix* *old-matrix* #'copy-cell)
     (game-step)
     (sdl:update-display)))))

(defun init-settings ()
  (setf *pixel-size* 1000)
  (setf *cells-count* 150)
  (setf *randomness* 2)
  (setf *cell-size* (/ *pixel-size* *cells-count*))
  (setf *matrix* (matrix-create *cells-count*))
  (setf *old-matrix* (matrix-create *cells-count*))
  (iterate #'(lambda (x y)
	       (matrix-set *matrix* x y (cell-create-random)))
	   *cells-count*)
  (matrix-iterate *matrix*
		  #'(lambda (x y c)
		      (when (cell-state c)
			(cell-near-update *matrix* x y #'add1))))
  (setf *dead-color* sdl:*black*)
  (setf *alive-color* (sdl:color :r 200 :g 0 :b 200)))


;;; Game of Life
;;; ============

(defun game-step ()
  (matrix-iterate *old-matrix*
		  #'(lambda (x y c)
		      (unless (cell-is-stable c)
			(draw-cell x y
				   (cell-update-state *matrix* x y c))))))

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

;;; Build
;;; =====

(defun build ()
  (ensure-directories-exist "bin/")
  (unless (cl-fad:file-exists-p "bin/SDL.dll")
    (cl-fad:copy-file "lib/SDL.dll" "bin/SDL.dll"))
  (save-application "bin/life.exe"
		    :toplevel-function #'main
		    :prepend-kernel t))
