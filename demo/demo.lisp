;;; -*- encoding: utf-8 -*-
(declaim (optimize (speed 0) (debug 3)))

(require :clx)
(load "../pstring.lisp")
(load "../faces.lisp")
(load "../xlib.lisp")


(defpackage :pstring-demo
  (:use #:cl #:pstrings))

(in-package :pstring-demo)

(defface :default
  :family "fixed"
  :pixel-size 14
  :foreground "white"
  :slant "r")

(defface :title1
  :pixel-size 24
  :foreground "red")

(defface :title2
  :pixel-size 20
  :inherit '(:title1))

(defface :title3
  :pixel-size 16)

(defface :emph
  :inherit '(:italic))

(defface :button
  :foreground "red"
  :family "helvetica"
  :pixel-size 14
  :inherit '(oblique bold))

(defparameter +button-text-margin+ 3)

(defstruct (button)
  window
  gcontext
  ascent
  text)

(defun create-button (parent x y text background-color)
  (let* ((win (xlib:create-window :parent parent
				 :x x
				 :y y
				 :background background-color
				 :width 1
				 :height 1
				 :event-mask (xlib:make-event-mask :button-press :button-release)
				 :depth (xlib:drawable-depth parent)))
	 (gcontext (xlib:create-gcontext :drawable win)))
    (multiple-value-bind (width height ascent)
	(xlib-pstring-extents gcontext text)
      (setf (xlib:drawable-width win) (+ (* 2 +button-text-margin+) width))
      (setf (xlib:drawable-height win) (+ (* 2 +button-text-margin+) height))
      (make-button :window win :text text :gcontext gcontext :ascent ascent ))))

(defun draw-button (button)
  (xlib-draw-pstring (button-text button)
		     (button-window button)
		     (button-gcontext button)
		     +button-text-margin+
		     (+ (button-ascent button) +button-text-margin+)))
			;;+button-text-margin+)))
		     
(defun destroy-button (button)
  (xlib:destroy-window (button-window button)))
    
;; (xlib-draw-pstring text win gcontext +button-text-margin+
;; (+ height +button-text-margin+)))))

(defun draw-text-lines (window context x y inter-line-spacing lines)
  (when lines
    (xlib-draw-pstring (car lines) window context x y)
    (draw-text-lines window context
		     x
		     (+ y  inter-line-spacing
			(second
			 (multiple-value-list
			  (xlib-pstring-extents context (car lines)))))
		     inter-line-spacing
		     (cdr lines))))
			    
(defvar *text*
  (list (pstring-propertize "xlib-draw-pstring-demo" :face :title1)
	"This is the demo for the xlib-draw-pstring function."
	"This text is being visualized using the xlib-draw-pstring function,"
	(pstring-concat "Only a few faces are defined, for titles and "
			(pstring-propertize
			 "emphasised text" :face :emph)
			".")))
  
(defun draw-pstring-demo (&optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (colormap (xlib:window-colormap root-window))
	 (win (xlib:create-window :parent root-window
				  :x 0
				  :y 0
				  :width 100
				  :height 100
				  :background black
				  :depth (xlib:drawable-depth root-window)
				  :event-mask (xlib:make-event-mask :exposure)))
	 (button (create-button win 0 0 (pstring-propertize "Exit" :face :button)
				white))
	 (gcontext (xlib:create-gcontext :drawable win))
	 exit)
    (xlib:map-window win)
    (xlib:map-subwindows win)
    (loop until exit do
	 (xlib:event-case (display)
	   (:exposure ()
		      (draw-text-lines win gcontext 10 40 5
				       *text*)
		      (draw-button button) t)
	   (:button-release () (setf exit t))))
    (xlib:destroy-window win)
    (destroy-button button)
    (xlib:close-display display)))
	
	 
	   
    
(defface input-box-prompt
    :foreground "cyan"
    :inherit '(bold))
(defvar *cursor-color* "magenta")

(defun color-cursor (text point)
  (let ((face (copy-face
	       (or (lookup-face
		    (pstring-get-property text :face point))
		   (lookup-face :default)))))
    (setf (face-property face :background) *cursor-color*)
    (pstring-put-property text point (1+ point) :face face)))

(defun input-box (prompt &optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (colormap (xlib:window-colormap root-window))
	 (win (xlib:create-window :parent root-window
				  :x 0
				  :y 0
				  :width 100
				  :background black
				  :height 100
				  :depth (xlib:drawable-depth root-window)
				  :event-mask (xlib:make-event-mask :exposure
								    :key-press
								    :button-press)))
	 (gcontext (xlib:create-gcontext :drawable win))
	 (prompt-text (pstring-propertize prompt :face :input-box-prompt))
	 (input-text "")
	 (other-text " ")
	 (margin 3)
	 (padding 3)
	 (point 0)
	 exit)
    (labels ((insert (char)
	       (setf input-text (concatenate 'string
					     (subseq input-text 0 point)
					     (string char)
					     (subseq input-text point)))
	       (incf point))
	     (redraw ()
	       (let ((text (pstring-concat prompt-text
					   (color-cursor input-text point)
					   other-text)))
		 (multiple-value-bind (w h)
		     (xlib-pstring-extents gcontext text)
		   (let ((ww (+ w (* 2 padding) (* 2 margin) 1))
			 (wh (+ h (* 2 padding) (* 2 margin) 1)))
		     (setf (xlib:drawable-width win) (1+ ww))
		     (setf (xlib:drawable-height win) (1+ wh))
		     (xlib:with-gcontext (gcontext :foreground black)
		      ;; (xlib:draw-rectangle win gcontext 1 1 (1- ww) (1- wh) t)
		       (xlib:draw-line win gcontext ww 0 ww wh)
		       )
		     (xlib:with-gcontext (gcontext :background black
						   :foreground
						   (xlib:alloc-color colormap
								     "green"))
		       (xlib:draw-rectangle win gcontext 0 0 ww wh))
		     )
		     
		   
		   (xlib-draw-pstring text
				      win gcontext
				      (+ margin padding 1)
				      (+ margin padding 1 h)
				      t)))))
      
      (xlib:map-window win)
      (xlib:map-subwindows win)
      (loop until exit do
	   (xlib:event-case (display)
	     (:exposure () (redraw)
			t)
			
	     (:key-press (code state)
			 (let ((char (xlib:keycode->character display code state)))
			   (when (characterp char)
			     (insert char)))
			 (redraw)
			 t)
	     
	     (:button-press (code)
			    (case code
			      (1 (setf exit t))
			      (2 (setf point (min (1+ point) (length input-text))))
			      (3 (setf point (max (1- point) 0))))
			    (redraw)
			    t)))
      (xlib:destroy-window win)
      (xlib:close-display display))))
			   
			 
		     
		     
  
