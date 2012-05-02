;;; faces.lisp --- 
;; 
;; Filename: faces.lisp
;; Description: Faces for propertized strings.
;; Author: Alessandro Piras
;; Created: Fri Apr  6 18:44:31 2012 (+0200)
;; URL: http://www.github.com/laynor/PStrings
;; Keywords: pstring, faces, emacs
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;  Some basic routines to work with faces. Faces can inherit from
;;  other faces and can be associated to names. All the faces inherit
;;  from the face named `default'
;; 
;;  The code provided
;;  here only deals with an abstract representaiton of faces,
;;  providing functionsfor managing face names, getting and setting
;;  the properties of a face, merging a face with its ancestors.
;;
;;  Different backends can have different face attributes, and even
;;  when they are the same, they should be treated in a different way
;;  when displaying the text.
;;  
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


;;; Faces: operations:
;;; Get/Set a property of a face
;;; Merge inherited faces
;;; Assoociate face with names
(in-package :pstrings)
(declaim (optimize (speed 0) (debug 3)))

(defvar *face-hash* (make-hash-table :test #'equal))

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
  "Retrieves the face named FACE-NAME from the face database."
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

(defun face-documentation (face)
  "Returns the documentation for FACE."
  (face-property face :documentation))

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
  "Sets the FACE-PROPERTIES of face FACE."
  (do* ((face-properties face-properties (cddr face-properties))
	(prop (first face-properties) (first face-properties))
	(value (second face-properties) (second face-properties)))
       ((null face-properties) face)
    (setf (face-property face prop) value)))
  
  
(defun make-face (&rest face-properties &key &allow-other-keys)
  "Returns a face object with the given FACE-PROPERTIES"
  (apply #'set-face-properties (make-empty-face) face-properties))

(defun defface-fn (face-name &rest face-props &key &allow-other-keys)
  (save-face (apply #'make-face face-props)
	     face-name))

(defmacro defface (face-name &body face-props &key &allow-other-keys)
  "Defines a face named FACE-NAME and stores its definition to the face database.
Example (taken from the xlib backend demo) :
   (defface :default
     :family \"fixed\"
     :pixel-size 14
     :foreground \"white\"
     :slant \"r\")
   
   (defface :title1
     :pixel-size 24
     :foreground \"red\")
   
   (defface :title2
     :pixel-size 20
     :inherit '(:title1))
   
   (defface :title3
     :pixel-size 16)
   
   (defface :emph
     :inherit '(:italic))"
  `(defface-fn ',face-name ,@face-props))

(defun copy-face (face)
  (copy-hash-table face))

(defun merge-faces (face1 face2)
  "Merges FACE1 and FACE2. For a property P, its value is taken from
FACE1 if it is defined, otherwise from face2."
  (let ((face (copy-face face1)))
    (maphash (lambda (key value)
	       (unless (gethash key face)
		 (setf (gethash key face) value)))
	     face2)
    face))


(defun merge-face-ancestors-1 (face)
  "Merges FACE with its ancestors."
  (let* ((ancestors (face-property face :inherit))
	 (face-1 (reduce #'merge-faces
			(cons (lookup-face face) (and ancestors (mapcar (compose #'merge-face-ancestors-1 #'lookup-face)
							  ancestors))))))
    (remhash :inherit face-1)
    face-1))
(defun merge-face-ancestors (face)
  "Merges FACE with its ancestors, using the face named `default' as the base ancestor."
  (let* ((ancestors (face-property face :inherit))
	 (face-1 (reduce #'merge-faces
			 (cons (lookup-face face)
			       (append
				(and ancestors
				     (mapcar (compose #'merge-face-ancestors-1 #'lookup-face)
					     ancestors))
				(list (lookup-face :default)))))))
    (remhash :inherit face-1)
    face-1))


(defface default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; faces.lisp ends here
