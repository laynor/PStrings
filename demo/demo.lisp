;;; -*- encoding: utf-8 -*-
(declaim (optimize (speed 0) (debug 3)))

(require :clx)
(require :pstrings-xlib)
(defpackage :pstring-demo
  (:use #:cl #:pstrings))

(in-package :pstring-demo)

(defface :default
  :family "fixed"
  :pixel-size 12
  :weight "medium"
  :foreground "white"
  :slant "r")

(defface :bold
  :weight "bold")
(defface :oblique
  :slant "o")
(defface :italic
  :slant "i")

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
    ;;(setf (xlib:window-override-redirect win) :ON)
    ;; (setf (xlib:window-priority win) :above)
    (xlib:map-window win)
    (xlib:map-subwindows win)
    (loop until exit do
	 (xlib:event-case (display :discard-p t)
	   (:exposure ()
		      (draw-text-lines win gcontext 10 40 5
				       *text*)
		      (draw-button button) t)
	   (:button-release () (setf exit t))))
    (xlib:destroy-window win)
    (destroy-button button)
    (xlib:close-display display)))

