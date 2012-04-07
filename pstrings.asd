(asdf:defsystem "pstrings"
  :description "pstrings: (clumsy) propertized strings (emacs like)."
  :version "0.1"
  :author "Alessandro Piras <laynor@gmail.com>"
  ;; :licence "MIT Licence"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
	       (:file "pstring")
	       (:file "faces")))
  
(defmethod operation-done-p ((o test-op) (c (eql (find-system :pstrings))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :pstrings))))
  (operate 'load-op :pstrings-tests)
  (operate 'test-op :pstrings-tests))
