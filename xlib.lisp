(ql:quickload "clx")
(in-package :sstring)


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

(defun xlib-face-font-select (context face &optional (n 0))
  (unless (> n 14)
    (handler-case (xlib:open-font
		   (xlib:gcontext-display context)
		   (xlib-face-font-name face n))
      (error () (xlib-face-font-select context face (1+ n))))))

(defun xlib-face-context-attributes (context colormap face)
  (let ((foreground `(xlib-face-foreground ,colormap ,face))
	(background `(xlib-face-background ,colormap ,face))
	(font `(xlib-face-font-select ,context ,face)))
    (list :foreground foreground :background background :font font)))

(defmacro xlib-with-face-attributes ((face context colormap) &body body)
  (once-only (face context colormap)
    (with-gensyms (f)
      `(let ((,f (and ,face (merge-face-ancestors (copy-face ,face)))))
	 (xlib:with-gcontext (context ,@(xlib-face-context-attributes context colormap f))
	   ,@body)))))
    
(defun xlib-draw-sstring (ss window context x y)
  (sstring-do-slices (ss sub props)
    (let* ((fname (getf props :face))
	   (face (lookup-face (or fname :default))))
      (xlib-with-face-attributes (face context (xlib:window-colormap window))
	(xlib:draw-glyphs window context x y sub)
	(incf x (xlib:text-extents context sub))))))

(defface default
  :family "helvetica"
  :pixel-size 14
  :slant "r")

(defface fixed
  :family "terminus")

(defface italic
  :slant "i")

(defface oblique
  :slant "o")

(defface bold
  :weight "bold")

(defface prova
  :foreground "magenta"
  :pixel-size 14
  :inherit '(fixed bold))

(export 'xlib-draw-string)
