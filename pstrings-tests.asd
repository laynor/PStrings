(asdf:defsystem "pstrings-tests"
  :description "Tests for the pstring library"
  :author "Alessandro Piras <laynor@gmail.com>"
  :version "0.1"
  :depends-on (:pstrings :fiveam)
  :components ((:file "tests")))
  
(defmethod operation-done-p ((o test-op) (c (eql (find-system :pstrings-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :pstrings-tests))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:pstrings-tests) args)))
    (run-tests)))

