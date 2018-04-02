;;;; move0x88.lisp has 0x88 specific move structures.

(in-package :tursas.state0x88)

(defstruct Move0x88
  (from 0 :type board-value)
  (to 0 :type board-value)
  (promotion 0 :type board-value))

(defun index->coord (index)
  "Converts given index to algebraic representation."
  (let* ((coord (format nil "~2,'0x" index))
         (num (1+ (- (char-code (schar coord 0)) 48)))
         (alpha (schar "abcdefgh" (- (char-code (schar coord 1)) 48))))
    (format nil "~a~a" alpha num)))

(defun coord->index (s)
  "Converts given string in coordinate
   representation to board index value."
  (when (valid-coord-p s)
    (let ((file (- (char-code (schar s 0)) 97))
          (rank (- (char-code (schar s 1)) 48)))
      (+ (* (1- rank) 16) file))))

(defun make-move (from to promotion)
  "Constructor for moves."
  (make-Move0x88 :from from :to to :promotion promotion))

(defun coord->move (s)
  "Make a Move from given string.
   The string should be in coordinate notation."
  (when (move-string-p s)
    (multiple-value-bind (from to promotion)
        (split-move s)
      (make-move (coord->index from)
                 (coord->index to)
                 (if promotion
                     (piece-value promotion)
                     0)))))

(defmethod move->coord ((move Move0x88))
  (strcat (index->coord (Move0x88-from move))
          (index->coord (Move0x88-to move))
          (let ((piece (piece-name (Move0x88-promotion move))))
            (unless (eql piece #\E)
              (string piece)))))

(defmethod from ((move Move0x88))
  (index->coord (Move0x88-from move)))

(defmethod to ((move Move0x88))
  (index->coord (Move0x88-to move)))

(defmethod promotion ((move Move0x88))
  (unless (zerop (Move0x88-promotion move))
    (piece-name (Move0x88-promotion move))))
