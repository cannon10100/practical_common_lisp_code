(in-package :spam)

;; Parameters

;; Maximum ham score
(defparameter *max-ham-score* .4)

;; Minimum spam score
(defparameter *min-spam-score* .6)

;; Hash table storing word features
(defvar *feature-database* (make-hash-table :test #'equal))

;; Total spam count
(defvar *total-spams* 0)

;; Total ham count
(defvar *total-hams* 0)

;; Classes

;; Class representing a word feature in the text of an email.
(defclass word-feature ()
  ((word
     :initarg :word
     :accessor word
     :initform (error "Must supply :word")
     :documentation "The word that this feature represents.")
   (spam-count
     :initarg :spam-count
     :accessor spam-count
     :initform 0
     :documentation "Number of spams we have seen this feature in.")
   (ham-count
     :initarg :ham-count
     :accessor ham-count
     :initform 0
     :documentation "Number of hams we have seen this feature in.")))

;; Functions

(defun classify (text)
  "Function to classify an email based on its text. Returns ham, spam, or unsure."
  (classification (score (extract-features text))))

(defun classification (score)
  "Function to classify an email given its spam score."
  (values 
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))

(defun clear-database ()
  "Function to clear the database of word features."
  (setf *feature-database* (make-hash-table :test #'equal)
        *total-spams* 0
        *total-hams* 0))

(defun intern-feature (word)
  "Function to get the word-feature corresponding to a word, creating it if it doesn't yet exist."
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  "Function to extract the words from the input text"
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
    :test #'string=))

(defun extract-features (text)
  "Function to get all word features from the input text"
  (mapcar #'intern-feature (extract-words text)))

(defun train (text type)
  "Function to train the classifier by incrementing the typed word counts for all words in the input text."
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  "Function to increment the type-specific count within the input word-feature."
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  "Function to increment the total count for the input type."
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun spam-probability (feature)
  "Function to calculate the probability that a feature is more common in spam than ham."
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional
                                  (assumed-probability 1/2)
                                  (weight 1))
  "Function to calculate the probability that a feature is more common in spam than ham, taking into account a uniform prior."
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features)
  "Function to score the input features and derive an overall 'spamminess' score"
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  "Function to check if a word-feature input is untrained"
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson"
  (inverse-chi-square
    (* -2 (reduce #'+ probs :key #'log))
    (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  "The inverse chi square function"
  (assert (evenp degrees-of-freedom))
  (min
    (loop with m = (/ value 2)
       for i below (/ degrees-of-freedom 2)
       for prob = (exp (- m)) then (* prob (/ m i))
       summing prob)
    1.0))

;; Methods

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))
