(defpackage :pstrings-tests
  (:use :cl :pstring :fiveam)
  (:import-from :pstring :set-equalp))

;;; TODO: add tests for faces

(in-package :pstrings-tests)
;;;; Tests
(def-suite pstring
    :description "Checking pstring related functions")

(defun ii (s e)
  "Returns a list containing all the intergers between S and E."
  (assert (> e s))
  (let (res)
    (do ((i s (1+ i)))
	((= i e) (nreverse res))
      (push i res))))

;;; Importing the set equalp predicate, we need it for the tests, but
;;; don't want it exported from pstring
;; (import 'pstring::set-equalp)

(in-suite pstring)

;;; Get all properties
(test pstring-get-properties
  (let ((s (pstring "foobarbaz" '(3 6 :face someface :otherprop othervalue))))
    (dolist (i (ii 3 6))
      (is (set-equalp (pstring-get-properties s i)
		      '(:face someface :otherprop othervalue))))
    (dolist (i (set-difference (ii 0 (pstring-length s))
			       (ii 3 6)))
      (is (null (pstring-get-properties s i))))))
    
;;; Get a property
(test pstring-get-property
  (let ((s (pstring "foobarbaz" '(3 6 :face someface :otherprop othervalue))))
    ;; match properties where defined
    (dolist (i (ii 3 6))
      (is (eq (pstring-get-property s :face i)
	      'someface))
      (is (eq (pstring-get-property s :otherprop i)
	      'othervalue))
      (is (null (pstring-get-property s :exampleprop i))))
    ;; properties should be null here
    (dolist (i (set-difference (ii 0 (pstring-length s))
			       (ii 3 6)))
      (is (null (pstring-get-property s :face i)))
      (is (null (pstring-get-property s :otherprop i))))))
  

;;; Test property put
(test pstring-put-property
  (let ((s (pstring "foobarbaz" '(3 4 :face foo))))
    (let ((sp (pstring-put-property s 2 4 :x 1)))
      (dolist (i (ii 2 4))
	(is (equalp (pstring-get-property sp :x i) 1)))
      (dolist (i (append (ii 0 2) (ii 4 (pstring-length s))))
	(is (null (pstring-get-property sp :x i)))))))

;;; PString equality not counting properties
(test pstring=
  (is (pstring= "foobar" (pstring "foobar")))
  (is (pstring= "foobar" (pstring "foobar" '(0 3 :face foo))))
  (is (not (pstring= "foobar" "foobaz")))
  (is (not (pstring= "foobar" (pstring "foobaz" '(0 3 :face foo))))))

;;; PString indexing
(test pstring-elt
  (is (char= (pstring-elt "foobar" 2) #\o))
  (is (char= (pstring-elt (pstring "foobar" '(0 3 :face foo)) 2) #\o)))

;;; PString iteration macro
(test pstring-do
  (let ((s (pstring "foobarbaz" '(3 6 :face foo))))
    (pstring-do (s char props index)
      (is (set-equalp props (pstring-get-properties s index)))
      (is (char= char (elt (pstring-string s) index))))))

;;; PString Equality, properties must match
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

;;; Substring extraction
(test pstring-substring
  (let ((s (pstring "foobarbaz" '(0 3 :face foo) '(3 6 :face bar))))
    (is (pstring-equal (pstring-substring s 2 4)
		       (pstring "ob" '(0 1 :face foo) '(1 2 :face bar))))))

;;; PString iteration per slice
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


(defun run-tests ()
  (run! 'pstring))
