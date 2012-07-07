;; import information from package.lisp
(in-package :com.ham.spam)

;; global variable definition
(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)
(defvar *feature-database* (make-hash-table :test #'equal))


(defun classify (text)
  classification (score (extract-features text)))

;;; Classifies the word
(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

;;; Keeps track of each word
(defclass word-feature ()
  
  ((word
    :initarg :word
    :accessor word
    :initform (error "must supply :word")
    :documentation "the word this file represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "number of hams we have seen this feature in.")))

;;; Resets feature-database (global hash table)
(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
	    (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))