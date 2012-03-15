(ql:quickload "fiveam")
(ql:quickload "alexandria")
(defpackage :sstring
  (:use #:cl #:alexandria
	#:fiveam))
(in-package :sstring)

;;; sstring-put-property (sstring start end prop value)
;;; sstring-get-properties (sstring pos)
;;; sstring-get-property (sstring pos prop)

;; (sstring "asdf" '(0 3 :face myface))
;; sstring representations) (= e1 e)) ;; add prop to existing intervals) (= e1 e)) ;; add prop to existing interval
;; 

(setf (symbol-function 'sstring) #'list)

(defun sstring-string (sstring)
  (if (stringp sstring) sstring (car sstring)))
(defun sstring-proplist (sstring)
  (if (stringp sstring) nil
      (cdr sstring)))

(defun prop-merge (plist p v)
  (let ((pl (copy-list plist)))
    (setf (getf pl p)
	  v)
    pl))

;;; Base cases:
;; s e [s1 e1] -> (s e p v) :: sprops
;; (s1/s) e e1 -> ((s e . sprops[p/v]) (e e1 . plist)) @ (cdr sprops)
;; (s1/s) e/e1 -> (s e . sprops[p/v]) :: (cdr sprops)
;;; Recursive cases
;; [s1 e1] s e -> (s1 e1 . plist) :: (sspp (cdr sprops) s e)
;; s s1 e1 e   -> (s s1 p v) :: (sspp sprops s1 e)
;; s s1 e e1   -> (s s1 p v) :: (sspp sprops s1 e)
;; s1 s e e1   -> (s1 s . plist) :: (sspp (s e1 . plist)::(cdr sprops) s e)
;; s1 s e1 e   -> (s1 s . plist) :: (sspp (s e1 . plist)::(cdr sprops) s e)
;; (s1/s) e1 e -> (s e1 . plist[p/v]) :: (sspp (cdr sprops) e1 e)

(defun sspp  (sprops s e p v)
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
		       (sspp (cdr sprops) s e p v)))
		((< s s1)
		 (cons `(,s ,s1 ,p ,v)
		       (sspp sprops s1 e p v)))
		((< s1 s)
		 (cons `(,s1 ,s . ,plist)
		       (sspp (cons `(,s ,e1 . ,plist) (cdr sprops))
			     s e p v)))
		((= s s1)
		 (cons `(,s ,e1 . ,(prop-merge plist p v))
		       (sspp (cdr sprops) e1 e p v))))))))

(defun sstring-put-property (sstring s e p v)
  (cons (sstring-string sstring)
	(sspp (sstring-proplist sstring) s e p v)))

(defun sstring-get-properties (sstring pos)
  (cddr (find-if #'(lambda (slice)
		     (and (<= (first slice) pos)
			  (< pos (second slice))))
		 (sstring-proplist sstring))))

(defun sstring-get-property (sstring prop pos)
  (getf (sstring-get-properties sstring pos)
	prop))


(defun sstring-length (sstring)
  (length (sstring-string sstring)))

(defun displace-slice (slice start end)
  (destructuring-bind (s e . p)
      slice
    (append (list (max 0 (- s start)) (- (min e end) start))
	    p)))

(defun sstring-substring (sstring start &optional end)
  (cond ((stringp sstring)
	 (subseq sstring start end))
	(t (let* ((string (subseq (sstring-string sstring) start end))
		  (props (remove-if (lambda (slice)
				      (or (>= (first slice) end)
					  (<= (second slice)
					      start)))
				    (sstring-proplist sstring)))
		  (displaced-props (mapcar (lambda (slice)
					     (displace-slice slice start end))
					   props)))
	     (cons string displaced-props)))))

;;;; Tests
(defun sstring-elt (sstring index)
  (elt (sstring-string sstring) index))

(defun sstring-do-bindings (sstring index char props)
  (append `((,char (sstring-elt ,sstring ,index)))
	  (and props `((,props (sstring-get-properties ,sstring ,index))))))

(defmacro sstring-do ((sstring char &optional props index result ) &body body)
  (once-only (sstring)
    (let* ((i (or index (gensym)))
	   (s (gensym)))
      `(let ((,s ,sstring))
	 (dotimes (,i (sstring::sstring-length ,sstring) ,result)
	   (let ,(sstring-do-bindings sstring i char props)
	     ,@body))))))
  
      
(defun sstring-slice-indexes (sstring)
  (remove-duplicates
   (append '(0)
	   (apply #'append (mapcar (lambda (slice)
				     (list (first slice) (second slice)))
				   (sstring-proplist sstring)))
	   (list (sstring-length sstring)))))

(defmacro sstring-do-slices ((sstring substring &optional props start end result) &body body)
  (once-only (sstring)
    (with-gensyms (sidxs ssub)
      (let ((s (or start (gensym)))
	    (e (or end (gensym)))
	    (props (or props (gensym))))
	`(do* ((,sidxs (sstring-slice-indexes ,sstring) (cdr ,sidxs))
	       (,s (first ,sidxs) ,e)
	       (,e (second ,sidxs) (second ,sidxs)))
	      ((null (cdr ,sidxs)) ,result)
	   (let* ((,ssub (sstring-substring ,sstring ,s ,e))
		  (,substring (car ,ssub))
		  (,props (cddr (cadr ,ssub)))) 
	     ,@body))))))

(defun sstring= (sstring1 sstring2)
  (string= (sstring-string sstring1)
	   (sstring-string sstring2)))

(defun set-equalp (s1 s2)
  (not (set-exclusive-or s1 s2 :test #'equalp)))

(defun sstring-equal (s1 s2)
  (and (sstring= s1 s2)
       (let (res)
	 (dotimes (i (sstring-length s1) (not (some #'null res)))
	   (push (set-equalp (sstring-get-properties s1 i)
			     (sstring-get-properties s2 i))
		 res)))))

(export 'sstring)
(export 'sstring-do)
(export 'sstring-do-slices)
(export 'sstring-get-property)
(export 'sstring-put-property)
(export 'sstring-put-properties)
(export 'sstring-elt)
(export 'sstring-substring)
(export 'sstring-length)
(export 'sstring=)

(def-suite sstring
    :description "Checking sstring related functions")

(defun ii (s e)
  (assert (> e s))
  (let (res)
    (do ((i s (1+ i)))
	((= i e) (nreverse res))
      (push i res))))






(in-suite sstring)

(test sstring-get-properties
  (let ((s (sstring "foobarbaz" '(3 6 :face someface :otherprop othervalue))))
    (dolist (i (ii 3 6))
      (is (set-equalp (sstring-get-properties s i)
		      '(:face someface :otherprop othervalue))))
    (dolist (i (set-difference (ii 0 (sstring-length s))
			       (ii 3 6)))
      (is (null (sstring-get-properties s i))))))
    
(test sstring-get-property
  (let ((s (sstring "foobarbaz" '(3 6 :face someface :otherprop othervalue))))
    (dolist (i (ii 3 6))
      (is (eq (sstring-get-property s :face i)
	      'someface))
      (is (eq (sstring-get-property s :otherprop i)
	      'othervalue))
      (is (null (sstring-get-property s :exampleprop i))))
    (dolist (i (set-difference (ii 0 (sstring-length s))
			       (ii 3 6)))
      (is (null (sstring-get-property s :face i)))
      (is (null (sstring-get-property s :otherprop i))))))
  

(test sstring-put-property
  (let ((s (sstring "foobarbaz" '(3 4 :face foo))))
    (let ((sp (sstring-put-property s 2 4 :x 1)))
      (dolist (i (ii 2 4))
	(is (equalp (sstring-get-property sp :x i) 1)))
      (dolist (i (append (ii 0 2) (ii 4 (sstring-length s))))
	(is (null (sstring-get-property sp :x i)))))))

(test sstring=
  (is (sstring= "foobar" (sstring "foobar")))
  (is (sstring= "foobar" (sstring "foobar" '(0 3 :face foo))))
  (is (not (sstring= "foobar" "foobaz")))
  (is (not (sstring= "foobar" (sstring "foobaz" '(0 3 :face foo))))))

(test sstring-elt
  (is (char= (sstring-elt "foobar" 2) #\o))
  (is (char= (sstring-elt (sstring "foobar" '(0 3 :face foo)) 2) #\o)))

(test sstring-do
  (let ((s (sstring "foobarbaz" '(3 6 :face foo))))
    (sstring-do (s char props index)
      (is (set-equalp props (sstring-get-properties s index)))
      (is (char= char (elt (sstring-string s) index))))))

(test sstring-equal
  (is (sstring-equal (sstring "foobarbaz") "foobarbaz"))
  (is (sstring-equal (sstring "foobarbaz" '(0 1 :face foo) '(1 2 :face foo))
		     (sstring "foobarbaz" '(0 2 :face foo))))
  (is (not (sstring-equal "foobar" "foobaz")))
  (is (not (sstring-equal (sstring "foobarbaz" '(0 1 :face foo))
			  (sstring "foobarbaz" '(0 2 :face foo)))))
			      
  (is (not (sstring-equal (sstring "foobarbaz" '(0 1 :face foo))
			  (sstring "foobarbaz" '(0 1 :face bar)))))
  
  (is (not (sstring-equal (sstring "foobarbaz" '(0 1 :face foo))
			  (sstring "foobarbaz" '(0 1 :face bar)))))
  (is (not (sstring-equal (sstring "foobarbaz" '(0 1 :face foo) '(1 2 :face bar))
			  (sstring "foobarbaz" '(0 1 :face foo))))))
  
    
    
(test sstring-substring
  (let ((s (sstring "foobarbaz" '(0 3 :face foo) '(3 6 :face bar))))
    (is (sstring-equal (sstring-substring s 2 4)
		       (sstring "ob" '(0 1 :face foo) '(1 2 :face bar))))))

(test sstring-do-slices
  (let ((s (sstring "foobarbaz" '(3 6 :face foo)))
	res)
    (sstring-do-slices (s sub props)
      (push (list sub props) res))
    (setq res (nreverse res))
    (is (= (length res) 3))
    (is (string= (car (first res)) "foo"))
    (is (string= (car (second res)) "bar"))
    (is (string= (car (third res)) "baz"))
    (is (set-equalp (cadr (first res)) nil))
    (is (set-equalp (cadr (second res)) '(:face foo)))
    (is (set-equalp (cadr (third res)) nil))))

