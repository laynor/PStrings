(ql:quickload "fiveam")
(ql:quickload "alexandria")

(defpackage :pstrings
  (:use #:cl #:alexandria
	#:fiveam))
(in-package :pstrings)

;;; pstring-put-property (pstring start end prop value)
;;; pstring-get-properties (pstring pos)
;;; pstring-get-property (pstring pos prop)

;; (pstring "asdf" '(0 3 :face myface))
;; pstring representations) (= e1 e)) ;; add prop to existing intervals) (= e1 e)) ;; add prop to existing interval
;; 

(setf (symbol-function 'pstring) #'list)

(defun pstring-string (pstring)
  (if (stringp pstring) pstring (car pstring)))
(defun pstring-proplist (pstring)
  (if (stringp pstring) nil
      (cdr pstring)))

(defun prop-merge (plist p v)
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
  (cons (pstring-string pstring)
	(pstring-put-property-helper (pstring-proplist pstring) s e p v)))

(defun pstring-put-properties (pstring s e &rest properties &key &allow-other-keys)
  (cond ((null properties) pstring)
	(t (apply #'pstring-put-properties (pstring-put-property pstring s e
								 (first properties)
								 (second properties))
		  s e 
		  (cddr properties)))))

(defun pstring-propertize (pstring &rest properties &key &allow-other-keys)
  (apply #'pstring-put-properties pstring 0 (pstring-length pstring)
	 properties))

(defun pstring-get-properties (pstring pos)
  (cddr (find-if #'(lambda (slice)
		     (and (<= (first slice) pos)
			  (< pos (second slice))))
		 (pstring-proplist pstring))))

(defun pstring-get-property (pstring prop pos)
  (getf (pstring-get-properties pstring pos)
	prop))


(defun pstring-length (pstring)
  (length (pstring-string pstring)))

(defun displace-slice (slice start end)
  (destructuring-bind (s e . p)
      slice
    (append (list (max 0 (- s start)) (- (min e end) start))
	    p)))

(defun pstring-substring (pstring start &optional end)
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
  (elt (pstring-string pstring) index))

(defun pstring-do-bindings (pstring index char props)
  (append `((,char (pstring-elt ,pstring ,index)))
	  (and props `((,props (pstring-get-properties ,pstring ,index))))))

(defmacro pstring-do ((pstring char &optional props index result ) &body body)
  (once-only (pstring)
    (let* ((i (or index (gensym)))
	   (s (gensym)))
      `(let ((,s ,pstring))
	 (dotimes (,i (pstrings::pstring-length ,pstring) ,result)
	   (let ,(pstring-do-bindings pstring i char props)
	     ,@body))))))
  
      
(defun pstring-slice-indexes (pstring)
  (remove-duplicates
   (append '(0)
	   (apply #'append (mapcar (lambda (slice)
				     (list (first slice) (second slice)))
				   (pstring-proplist pstring)))
	   (list (pstring-length pstring)))))

(defmacro pstring-do-slices ((pstring substring &optional props start end result) &body body)
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
  (string= (pstring-string pstring1)
	   (pstring-string pstring2)))

(defun set-equalp (s1 s2)
  (not (set-exclusive-or s1 s2 :test #'equalp)))

(defun pstring-equal (s1 s2)
  (and (pstring= s1 s2)
       (let (res)
	 (dotimes (i (pstring-length s1) (not (some #'null res)))
	   (push (set-equalp (pstring-get-properties s1 i)
			     (pstring-get-properties s2 i))
		 res)))))

(defun pstring-concat-2 (s1 s2)
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
  (reduce #'pstring-concat-2 pstrings))

;;;; Exports 
(export 'pstring)
(export 'pstring-do)
(export 'pstring-do-slices)
(export 'pstring-get-property)
(export 'pstring-put-property)
(export 'pstring-put-properties)
(export 'pstring-propertize)
(export 'pstring-elt)
(export 'pstring-substring)
(export 'pstring-concat)
(export 'pstring-length)
(export 'pstring=)

;;;; Tests
(def-suite pstring
    :description "Checking pstring related functions")

(defun ii (s e)
  (assert (> e s))
  (let (res)
    (do ((i s (1+ i)))
	((= i e) (nreverse res))
      (push i res))))






(in-suite pstring)

(test pstring-get-properties
  (let ((s (pstring "foobarbaz" '(3 6 :face someface :otherprop othervalue))))
    (dolist (i (ii 3 6))
      (is (set-equalp (pstring-get-properties s i)
		      '(:face someface :otherprop othervalue))))
    (dolist (i (set-difference (ii 0 (pstring-length s))
			       (ii 3 6)))
      (is (null (pstring-get-properties s i))))))
    
(test pstring-get-property
  (let ((s (pstring "foobarbaz" '(3 6 :face someface :otherprop othervalue))))
    (dolist (i (ii 3 6))
      (is (eq (pstring-get-property s :face i)
	      'someface))
      (is (eq (pstring-get-property s :otherprop i)
	      'othervalue))
      (is (null (pstring-get-property s :exampleprop i))))
    (dolist (i (set-difference (ii 0 (pstring-length s))
			       (ii 3 6)))
      (is (null (pstring-get-property s :face i)))
      (is (null (pstring-get-property s :otherprop i))))))
  

(test pstring-put-property
  (let ((s (pstring "foobarbaz" '(3 4 :face foo))))
    (let ((sp (pstring-put-property s 2 4 :x 1)))
      (dolist (i (ii 2 4))
	(is (equalp (pstring-get-property sp :x i) 1)))
      (dolist (i (append (ii 0 2) (ii 4 (pstring-length s))))
	(is (null (pstring-get-property sp :x i)))))))

(test pstring=
  (is (pstring= "foobar" (pstring "foobar")))
  (is (pstring= "foobar" (pstring "foobar" '(0 3 :face foo))))
  (is (not (pstring= "foobar" "foobaz")))
  (is (not (pstring= "foobar" (pstring "foobaz" '(0 3 :face foo))))))

(test pstring-elt
  (is (char= (pstring-elt "foobar" 2) #\o))
  (is (char= (pstring-elt (pstring "foobar" '(0 3 :face foo)) 2) #\o)))

(test pstring-do
  (let ((s (pstring "foobarbaz" '(3 6 :face foo))))
    (pstring-do (s char props index)
      (is (set-equalp props (pstring-get-properties s index)))
      (is (char= char (elt (pstring-string s) index))))))

(test pstring-equal
  (is (pstring-equal (pstring "foobarbaz") "foobarbaz"))
  (is (pstring-equal (pstring "foobarbaz" '(0 1 :face foo) '(1 2 :face foo))
		     (pstring "foobarbaz" '(0 2 :face foo))))
  (is (not (pstring-equal "foobar" "foobaz")))
  (is (not (pstring-equal (pstring "foobarbaz" '(0 1 :face foo))
			  (pstring "foobarbaz" '(0 2 :face foo)))))
			      
  (is (not (pstring-equal (pstring "foobarbaz" '(0 1 :face foo))
			  (pstring "foobarbaz" '(0 1 :face bar)))))
  
  (is (not (pstring-equal (pstring "foobarbaz" '(0 1 :face foo))
			  (pstring "foobarbaz" '(0 1 :face bar)))))
  (is (not (pstring-equal (pstring "foobarbaz" '(0 1 :face foo) '(1 2 :face bar))
			  (pstring "foobarbaz" '(0 1 :face foo))))))
  
    
    
(test pstring-substring
  (let ((s (pstring "foobarbaz" '(0 3 :face foo) '(3 6 :face bar))))
    (is (pstring-equal (pstring-substring s 2 4)
		       (pstring "ob" '(0 1 :face foo) '(1 2 :face bar))))))

(test pstring-do-slices
  (let ((s (pstring "foobarbaz" '(3 6 :face foo)))
	res)
    (pstring-do-slices (s sub props)
      (push (list sub props) res))
    (setq res (nreverse res))
    (is (= (length res) 3))
    (is (string= (car (first res)) "foo"))
    (is (string= (car (second res)) "bar"))
    (is (string= (car (third res)) "baz"))
    (is (set-equalp (cadr (first res)) nil))
    (is (set-equalp (cadr (second res)) '(:face foo)))
    (is (set-equalp (cadr (third res)) nil))))

