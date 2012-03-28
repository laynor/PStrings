;;; Faces: operations:
;;; Get/Set a property of a face
;;; Merge inherited faces
;;; Assoociate face with names
(in-package :pstrings)

(defparameter *face-hash* (make-hash-table :test #'equal))

;;; Save the association name->face
(defgeneric save-face (face name)
  (:documentation "Associates a face with a name"))
(defmethod save-face (face (name string))
  (setf (gethash (string-downcase name) *face-hash*)
	face))
(defmethod save-face (face (name symbol))
  (save-face face (symbol-name name)))
;;; Lookup a face by name
(defun lookup-face (face-name)
  (etypecase face-name
    ((or string symbol) (gethash (string-downcase
				  (if (stringp face-name)
				      face-name
				      (symbol-name face-name)))
				 *face-hash*))
    (hash-table face-name)))

;;; Access face properties
(defgeneric face-property (face property)
  (:documentation "Returns a face property. Setf-able."))
(defmethod face-property ((face hash-table) property)
  (gethash property face))
(defmethod face-property ((face symbol) property)
  (gethash property (lookup-face face)))
(defmethod face-property ((face string) property)
  (gethash property (lookup-face face)))

(defmethod (setf face-property) (value (face hash-table) property)
  (setf (gethash property face) value))
(defmethod (setf face-property) (value (face string) property)
  (setf (face-property (lookup-face face) property) value))
(defmethod (setf face-property) (value (face symbol) property)
  (setf (face-property (lookup-face face) property) value))
   
;;; Create faces
(defun make-empty-face ()
  (make-hash-table))

(defun set-face-properties (face &rest face-properties &key &allow-other-keys)
  (do* ((face-properties face-properties (cddr face-properties))
	(prop (first face-properties) (first face-properties))
	(value (second face-properties) (second face-properties)))
       ((null face-properties) face)
    (setf (face-property face prop) value)))
  
  
(defun make-face (&rest face-properties &key &allow-other-keys)
  (apply #'set-face-properties (make-empty-face) face-properties))

(defun defface-fn (face-name &rest face-props &key &allow-other-keys)
  (save-face (apply #'make-face face-props)
	     face-name))

(defmacro defface (face-name &body face-props &key &allow-other-keys)
  `(defface-fn ',face-name ,@face-props))

(defun copy-face (face)
  (copy-hash-table face))

(defun merge-faces (face1 face2)
  (let ((face (copy-face face1)))
    (maphash (lambda (key value)
	       (unless (gethash key face)
		 (setf (gethash key face) value)))
	     face2)
    face))


(defun merge-face-ancestors-1 (face)
  (let ((face (reduce #'merge-faces
		      (cons face (mapcar (compose #'merge-face-ancestors-1 #'lookup-face)
					  (face-property face :inherit))))))
    (remhash :inherit face)
    face))
(defun merge-face-ancestors (face)
  (let ((face (reduce #'merge-faces
		      (cons face
			    (append
			     (mapcar (compose #'merge-face-ancestors-1 #'lookup-face)
				     (face-property face :inherit))
			     (list (lookup-face :default)))))))
    (remhash :inherit face)
    face))

(export 'defface)
(export 'copy-face)
(export 'make-face)
(export 'face-property)
(export 'set-face-properties)
(export 'lookup-face)

(defface default)


