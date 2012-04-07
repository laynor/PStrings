(defpackage :pstrings
  (:use #:cl #:alexandria)
  (:nicknames :pstr :pstring)
  (:export #:pstring
	   #:pstring-string
	   #:pstring-proplist
	   #:pstring-do
	   #:pstring-do-slices
	   #:pstring-put-property
	   #:pstring-get-properties
	   #:pstring-get-property
	   #:pstring-propertize
	   #:pstring-elt
	   #:pstring-substring
	   #:pstring-concat
	   #:pstring-length
	   #:pstring=
	   #:pstring-equal
	   #:defface
	   #:copy-face
	   #:make-face
	   #:face-property
	   #:set-face-properties
	   #:lookup-face))
