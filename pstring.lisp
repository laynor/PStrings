;;;; pstring.lisp --- 
;;; 
;;; Filename: pstring.lisp
;;; Description: Core code for propertized string
;;; Author: Alessandro Piras
;;; Maintainer: 
;;; Created: Fri Apr  6 18:40:55 2012 (+0200)
;;; URL: http://www.github.com/laynor/PStrings
;;; Keywords: strings, emacs
;;; Compatibility: 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Commentary: 
;;;  An incomplete and somewhat clumsy implementation of propertized
;;;  strings in Common Lisp.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Change Log:
;;;  
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Code:



(in-package :pstrings)

;;; PString construction function 
(setf (symbol-function 'pstring) #'list)

(defun pstring-string (pstring)
  "Returns the string portion of PSTRING."
  (if (stringp pstring) pstring (car pstring)))
(defun pstring-proplist (pstring)
  "Returns the property intervals of PSTRING."
  (and (listp pstring) (cdr pstring)))

(defun prop-merge (plist p v)
  "Returns a copy of PLIST where the property P set to V."
  (let ((pl (copy-list plist)))
    (setf (getf pl p)
	  v)
    pl))

;;; pstring put property helper 
;;; Base cases:
;; s e [s1 e1] -> (s e p v) :: sprops
;; (s1/s) e e1 -> ((s e . sprops[p/v]) (e e1 . plist)) @ (cdr sprops)
;; (s1/s) e/e1 -> (s e . sprops[p/v]) :: (cdr sprops)
;;; Recursive cases
;; [s1 e1] s e -> (s1 e1 . plist) :: (pstring-put-property-helper (cdr sprops) s e)
;; s s1 e1 e   -> (s s1 p v) :: (pstring-put-property-helper sprops s1 e)
;; s s1 e e1   -> (s s1 p v) :: (pstring-put-property-helper sprops s1 e)
;; s1 s e e1   -> (s1 s . plist) :: (pstring-put-property-helper (s e1 . plist)::(cdr sprops) s e)
;; s1 s e1 e   -> (s1 s . plist) :: (pstring-put-property-helper (s e1 . plist)::(cdr sprops) s e)
;; (s1/s) e1 e -> (s e1 . plist[p/v]) :: (pstring-put-property-helper (cdr sprops) e1 e)

(defun pstring-put-property-helper  (sprops s e p v)
  (let ((slice (car sprops)))
    (if (null slice)
	`((,s ,e ,p ,v))
	(destructuring-bind (s1 e1 . plist)
	    slice
	  (cond ((< e s1)
		 (cons `(,s ,e ,p ,v) sprops))
		((and (= s s1) (< e e1))
		 (nconc (list `(,s ,e . ,(prop-merge plist p v))
			      `(,e ,e1 . ,plist))
			(cdr sprops)))
		((and (= s s1) (= e e1))
		 (cons `(,s ,e . ,(prop-merge plist p v))
		       (cdr sprops)))
		((<= e1 s)
		 (cons `(,s1 ,e1 . ,plist)
		       (pstring-put-property-helper (cdr sprops) s e p v)))
		((< s s1)
		 (cons `(,s ,s1 ,p ,v)
		       (pstring-put-property-helper sprops s1 e p v)))
		((< s1 s)
		 (cons `(,s1 ,s . ,plist)
		       (pstring-put-property-helper (cons `(,s ,e1 . ,plist) (cdr sprops))
			     s e p v)))
		((= s s1)
		 (cons `(,s ,e1 . ,(prop-merge plist p v))
		       (pstring-put-property-helper (cdr sprops) e1 e p v))))))))

(defun pstring-put-property (pstring s e p v)
  "Sets one property of PSTRING from S (start) to E (end).
The fourth and fifth arguments P and V specify the property to set."
  (cons (pstring-string pstring)
	(pstring-put-property-helper (pstring-proplist pstring) s e p v)))

(defun pstring-put-properties (pstring s e &rest properties &key &allow-other-keys)
  "Sets any number of properties of PSTRING from S (start) to E (end).
The properties are specified as a sequence of PROPERTY VALUE pairs, like for example
(pstring-put-properties \"asfdasdf\" 0 3 :face :myface :property value :property2 value2)."
  (cond ((null properties) pstring)
	(t (apply #'pstring-put-properties (pstring-put-property pstring s e
								 (first properties)
								 (second properties))
		  s e 
		  (cddr properties)))))

(defun pstring-propertize (pstring &rest properties &key &allow-other-keys)
  "Sets a number of PROPERTIES of PSTRING to the specified values,
from its beginning to its end."
  (apply #'pstring-put-properties pstring 0 (pstring-length pstring)
	 properties))

(defun pstring-get-properties (pstring pos)
  "Returns the properties of PSTRING at position POS."
  (cddr (find-if #'(lambda (slice)
		     (and (<= (first slice) pos)
			  (< pos (second slice))))
		 (pstring-proplist pstring))))

(defun pstring-get-property (pstring prop pos)
  "Returns the value of the property PROP of PSTRING at the position POS."
  (getf (pstring-get-properties pstring pos)
	prop))


(defun pstring-length (pstring)
  "Returns the lenght of PSTRING."
  (length (pstring-string pstring)))

(defun displace-slice (slice start end)
  "Returns a slice with the same properties as SLICE, but with START and END relative to a possible
substring starting at START and ending at END."
  (destructuring-bind (s e . p)
      slice
    (append (list (max 0 (- s start)) (- (min e end) start))
	    p)))

(defun pstring-substring (pstring start &optional end)
  "Returns a substring of PSTRING, starting at START and ending at END."
  (cond ((stringp pstring)
	 (subseq pstring start end))
	(t (let* ((string (subseq (pstring-string pstring) start end))
		  (props (remove-if (lambda (slice)
				      (or (>= (first slice) end)
					  (<= (second slice)
					      start)))
				    (pstring-proplist pstring)))
		  (displaced-props (mapcar (lambda (slice)
					     (displace-slice slice start end))
					   props)))
	     (cons string displaced-props)))))

;;;; Tests
(defun pstring-elt (pstring index)
  "Returns the character ot index INDEX of PSTRING."
  (elt (pstring-string pstring) index))

;;; helper for the pstring do macro
(defun pstring-do-bindings (pstring index char props)
  (append `((,char (pstring-elt ,pstring ,index)))
	  (and props `((,props (pstring-get-properties ,pstring ,index))))))

(defmacro pstring-do ((pstring char &optional props index result ) &body body)
  "Iterates over the characters of PSTRING, left to right.
CHAR is bound to the character being iterated
PROPS is bound to a plist containing the properties defined for CHAR
INDEX is bound to the index of the current iteration
RESULT can be bound to a form whose result will be returned at the end of the loop."
  (once-only (pstring)
    (let* ((i (or index (gensym)))
	   (s (gensym)))
      `(let ((,s ,pstring))
	 (dotimes (,i (pstrings::pstring-length ,pstring) ,result)
	   (let ,(pstring-do-bindings pstring i char props)
	     ,@body))))))
  
      
;;; Helper for pstring-do-slices, returns a list of indexes at which
;;; there is a change in the properties of PSTRING
(defun pstring-slice-indexes (pstring)
  (remove-duplicates
   (append '(0)
	   (apply #'append (mapcar (lambda (slice)
				     (list (first slice) (second slice)))
				   (pstring-proplist pstring)))
	   (list (pstring-length pstring)))))

(defmacro pstring-do-slices ((pstring substring &optional props start end result) &body body)
  "Iterates over the slices of PSTRING.
SUBSTRING is bound to the substring starting at START and ending at END in PSTRING
PROPS is bound to a plist containing the properties active for SUBSTRING
RESULT can be bound to a form whose result will be returned at the end of the loop."
  (once-only (pstring)
    (with-gensyms (sidxs ssub)
      (let ((s (or start (gensym)))
	    (e (or end (gensym)))
	    (props (or props (gensym))))
	`(do* ((,sidxs (pstring-slice-indexes ,pstring) (cdr ,sidxs))
	       (,s (first ,sidxs) ,e)
	       (,e (second ,sidxs) (second ,sidxs)))
	      ((null (cdr ,sidxs)) ,result)
	   (let* ((,ssub (pstring-substring ,pstring ,s ,e))
		  (,substring (pstring-string ,ssub))
		  (,props (cddr (car (pstring-proplist ,ssub)))) )
	     ,@body))))))

(defun pstring= (pstring1 pstring2)
  "Returns T if pstring1 and pstring2 are string=. Properties are not taken into account."
  (string= (pstring-string pstring1)
	   (pstring-string pstring2)))

(defun set-equalp (s1 s2)
  "Returns T if S1 and S2 contain the same elements. The elements are tested using equalp."
  (not (set-exclusive-or s1 s2 :test #'equalp)))

(defun pstring-equal (s1 s2)
  "Returns T if S1 and S2 have the same properties if S1 and S2 are
pstring= and they have the same properties."
  (and (pstring= s1 s2)
       (let (res)
	 (dotimes (i (pstring-length s1) (not (some #'null res)))
	   (push (set-equalp (pstring-get-properties s1 i)
			     (pstring-get-properties s2 i))
		 res)))))

(defun pstring-concat-2 (s1 s2)
  "Concatenates S1 and S2."
  (let* ((str (concatenate 'string
			   (pstring-string s1)
			   (pstring-string s2)))
	 (len (pstring-length s1))
	 (plist (append (pstring-proplist s1)
			(mapcar (lambda (el)
				  (destructuring-bind (s e . p) el
				    `(,(+ s len) ,(+ e len) . ,p)))
				(pstring-proplist s2)))))
    (cons str plist)))
				       
(defun pstring-concat (&rest pstrings)
  "Concatenates PSTRINGS."
  (reduce #'pstring-concat-2 pstrings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pstring.lisp ends here
