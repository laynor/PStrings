(in-package :pstrings)


(defun xlib-font-name (n &key (foundry "*") (family "*") (weight "*") (slant "*")
			 (width "*") (style "*") (pixel-size "*") (point-size "*")
			 (resolution-x "*") (resolution-y "*") (spacing "*")
			 (average-width "*") (registry "*") (encoding "*")
			 &allow-other-keys)
  (declare (ignorable foundry family weight slant width style pixel-size point-size
		      resolution-x resolution-y spacing average-width registry encoding))
  (format nil "~{-~a~}"
	  (append
	   (butlast
	    (list foundry family weight slant width style pixel-size point-size
		  resolution-x resolution-y spacing average-width registry encoding)
	    n)
	   (make-list n :initial-element "*"))))
	  
  
(defun xlib-face-font-name (face &optional (n 0))
  (apply #'xlib-font-name n (hash-table-plist face)))


(defun xlib-face-color (colormap face property)
  (ignore-errors
    (xlib:alloc-color colormap (face-property face property))))

(defun xlib-face-foreground (colormap face)
  (xlib-face-color colormap face :foreground))

(defun xlib-face-background (colormap face)
  (xlib-face-color colormap face :background))

(defun xlib-face-simple-resolve-font (display face &optional (n 0))
  (unless (> n 14)
    (handler-case (xlib:open-font
		   display
		   (xlib-face-font-name face n))
      (error () (xlib-face-simple-resolve-font display face (1+ n))))))
  
(defvar *xlib-face-font-solver* #'xlib-face-simple-resolve-font
  "Function used by the xlib pstring routines for face->font resolution.")

(defun xlib-face-font-select (context face)
  (funcall *xlib-face-font-solver*
	   (xlib:gcontext-display context)
	   face))

(defun xlib-face-context-attributes (context colormap face)
  (let ((foreground `(and ,colormap (xlib-face-foreground ,colormap ,face)))
	(background `(and ,colormap (xlib-face-background ,colormap ,face)))
	(font `(xlib-face-font-select ,context ,face)))
    (list :foreground foreground :background background :font font)))

(defmacro xlib-with-face-attributes ((face context colormap) &body body)
  (once-only (face context colormap)
    (with-gensyms (f)
      `(let ((,f (and ,face (merge-face-ancestors (copy-face ,face)))))
	 (xlib:with-gcontext (,context ,@(xlib-face-context-attributes context colormap f))
	   ,@body)))))
    
(defun xlib-draw-pstring (ss window context x y &optional draw-image-glyphs-p)
  (pstring-do-slices (ss sub props)
    (let* ((fname (getf props :face))
	   (face (lookup-face (or fname :default))))
      (xlib-with-face-attributes (face context (xlib:window-colormap window))
	(if draw-image-glyphs-p
	    (xlib:draw-image-glyphs window context x y sub)
	    (xlib:draw-glyphs window context x y sub))
	(incf x (xlib:text-extents context sub))))))

(defun xlib-pstring-extents (gcontext pstring)
  (let ((width 0) (height 0) (ascent 0) left (right 0))
    (pstring-do-slices (pstring sub props)
      (let* ((fname (getf props :face))
	     (face (lookup-face (or fname :default))))
	(xlib-with-face-attributes (face gcontext nil)
	  (print (multiple-value-list (xlib:text-extents gcontext sub)))
	  (multiple-value-bind (w a d l r fa fd)
	      (xlib:text-extents gcontext sub)
	    (setf left (or left l))
	    (setf right r)
	    (incf width w)
	    (setf ascent (max ascent a))
	    (setf height (max height (+ fa fd)))))))
    (values width height ascent )))
	    
      

(defface default
  :foreground "white"
  :family "helvetica"
  :pixel-size 12
  :weight "medium"
  :slant "r")

(defface fixed
  :family "terminus")

(defface italic
  :slant "i")

(defface oblique
  :slant "o")

(defface bold
  :weight "bold")


(export 'xlib-draw-pstring)
(export 'xlib-pstring-extents)
