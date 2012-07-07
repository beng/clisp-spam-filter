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