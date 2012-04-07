(in-package :pstrings)


(defun xlib-font-name (n &key (foundry "*") (family "*") (weight "*") (slant "*")
			 (width "*") (style "*") (pixel-size "*") (point-size "*")
			 (resolution-x "*") (resolution-y "*") (spacing "*")
			 (average-width "*") (registry "*") (encoding "*")
			 &allow-other-keys)
  "Builds a font name from its key arguments.
The argument N specifies how many parameters to ignore, starting from the last, in this order:
	    foundry
	    family
	    weight
	    slant
	    width
	    style
	    pixel-size
	    point-size
	    resolution-x
	    resolution-y
	    spacing
	    average-width
	    registry
	    encoding"
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
  "Returns the font name used to display text with FACE. 
The optional argument N specifies how many parameters to ignore,
starting from the last, in this order:
	    foundry
	    family
	    weight
	    slant
	    width
	    style
	    pixel-size
	    point-size
	    resolution-x
	    resolution-y
	    spacing
	    average-width
	    registry
	    encoding"
  (apply #'xlib-font-name n (hash-table-plist face)))


(defun xlib-face-color (colormap face property)
  "Returns a CLX color object built from a PROPERTY of FACE, given a COLORMAP"
  (ignore-errors
    (xlib:alloc-color colormap (face-property face property))))

(defun xlib-face-foreground (colormap face)
  "Returns the foreground color of FACE, given COLORMAP, as a CLX color object."
  (xlib-face-color colormap face :foreground))

(defun xlib-face-background (colormap face)
  "Returns the background color of FACE, given COLORMAP, as a CLX color object."
  (xlib-face-color colormap face :background))

;;; Default font solver
(defun xlib-face-simple-resolve-font (display face &optional (n 0))
  "Returns the font name used to display text propertized with FACE.
The font name is resolved incrementally ignoring the font properties,
starting from the full specification and ending with
-*-*-*-*-*-*-*-*-*-*-*-*-*-* if no matching font can be found."
  (unless (> n 14)
    (handler-case (xlib:open-font
		   display
		   (xlib-face-font-name face n))
      (error () (xlib-face-simple-resolve-font display face (1+ n))))))
  
(defvar *xlib-face-font-solver* #'xlib-face-simple-resolve-font
  "Function used by the xlib pstring routines for face->font resolution.")

(defun xlib-face-font-select (gcontext face)
  "Returns the name of the font used to display text propertized with
FACE using the graphic context GCONTEXT, using
`*xlib-face-font-solver*' to resolve the font name."
  (funcall *xlib-face-font-solver*
	   (xlib:gcontext-display gcontext)
	   face))

;;; Helper for the xlib-with-face-attributes macro
(eval-when (:compile-toplevel)
  (defun xlib-face-gcontext-attributes (gcontext colormap face)
    (let ((foreground `(and ,colormap (xlib-face-foreground ,colormap ,face)))
	  (background `(and ,colormap (xlib-face-background ,colormap ,face)))
	  (font `(xlib-face-font-select ,gcontext ,face)))
      (list :foreground foreground :background background :font font))))

(defmacro xlib-with-face-attributes ((face gcontext colormap) &body body)
  "Sets up GCONTEXT so that text drawn with xlib:draw-image-glyphs and
  xlib:draw-glyphs in BODY is printed according to FACE's attributes."
  (once-only (face gcontext colormap)
    (with-gensyms (f)
      `(let ((,f (and ,face (merge-face-ancestors (copy-face ,face)))))
	 (xlib:with-gcontext (,gcontext ,@(xlib-face-gcontext-attributes gcontext colormap f))
	   ,@body)))))
    
(defun xlib-draw-pstring (pstring window gcontext x y &optional draw-image-glyphs-p)
  "Draws PSTRING on WINDOW at coordinates X, Y using GCONTEXT.
The optional argument DRAW-IMAGE-GLYPHS-P controls whether the text is
drawn using `xlib:draw-glyphs' (default) or `xlib:draw-image-glyphs'."
  (pstring-do-slices (pstring sub props)
    (let* ((fname (getf props :face))
	   (face (lookup-face (or fname :default))))
      (xlib-with-face-attributes (face gcontext (xlib:window-colormap window))
	(if draw-image-glyphs-p
	    (xlib:draw-image-glyphs window gcontext x y sub)
	    (xlib:draw-glyphs window gcontext x y sub))
	(incf x (xlib:text-extents gcontext sub))))))

(defun xlib-pstring-extents (gcontext pstring)
  "Returns the width, height and ascent of PSTRING, as it would be drawn using GCONTEXT."
  (let ((width 0) (height 0) (ascent 0) left (right 0))
    (pstring-do-slices (pstring sub props)
      (let* ((fname (getf props :face))
	     (face (lookup-face (or fname :default))))
	(xlib-with-face-attributes (face gcontext nil)
	  (multiple-value-list (xlib:text-extents gcontext sub))
	  (multiple-value-bind (w a d l r fa fd)
	      (xlib:text-extents gcontext sub)
	    (setf left (or left l))
	    (setf right r)
	    (incf width w)
	    (setf ascent (max ascent a))
	    (setf height (max height (+ fa fd)))))))
    (values width height ascent )))

;;; EXPORTS
(export 'xlib-draw-pstring)
(export 'xlib-pstring-extents)
