(in-package :tursas.state0x88)

(defstruct Move0x88
  from to promotion)

(defun index->coord (index)
  "Converts given index to algebraic representation."
  (let* ((coord (format nil "~X" index))
         (num (+ (- (char-code (schar coord 0)) 48) 1))
         (alpha (schar "abcdefgh" (- (char-code (schar coord 1)) 48))))
    (format nil "~a~a" alpha num)))

(defun coord->index (s)
  "Converts given string in coordinate
   representation to board index value."
  (when (valid-coord-p s)
    (let ((file (- (char-code (schar s 0)) 97))
          (rank (- (char-code (schar s 1)) 48)))
      (+ (* (- rank 1) 16) file))))

(defun make-move (from to promotion)
  "Constructor for moves."
  (make-Move0x88 :from from :to to :promotion promotion))

(defun coord->move (s)
  "Make a Move from given string.
   The string should be in coordinate notation."
  (when (move-string-p s)
    (let ((parts (split-move s)))
      (make-move (coord->index (first parts))
                 (coord->index (second parts))
                 (if (= (list-length parts) 3)
                     (piece-value (aref s 4))
                     0)))))

(defmethod move->coord ((move Move0x88))
  (concatenate 'string
               (index->coord (Move0x88-from move))
               (index->coord (Move0x88-to move))
               (let ((piece (piece-name (Move0x88-promotion move))))
                 (when (not (eql piece #\E))
                   piece))))

(defmethod from ((move Move0x88))
  (index->coord (Move0x88-from move)))

(defmethod to ((move Move0x88))
  (index->coord (Move0x88-to move)))

(defmethod promotion ((move Move0x88))
  (when (not (zerop (Move0x88-promotion move)))
    (piece-name (Move0x88-promotion move))))
