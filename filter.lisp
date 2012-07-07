(in-package :com.ham.spam)

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classify (text)
  classification (score (extract-features text)))

(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

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