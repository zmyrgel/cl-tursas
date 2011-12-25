(in-package :tursas)

(defconstant valid-coords
  (apply #'append
         (loop :for x :across "abcdefgh"
               :collect (loop :for y :below 8 :collect (format nil "~a~a" x (1+ y)))))
  "List of valid chess coordinate strings.")

(defun valid-coord-p (s)
  "Checks if given string s is a valid chess board coordinate."
  (some (lambda (coord)
          (equalp coord s)) valid-coords))

(defun split-move (algebraic)
  "Partitions chess move given in coordinate notation to pair of coordinates
   and possible promotion character."
  (case (length algebraic)
    (4 (list (subseq algebraic 0 2)
             (subseq algebraic 2)))
    (5 (list (subseq algebraic 0 2)
             (subseq algebraic 2 4)
             (subseq algebraic 4)))
    (otherwise 0)))

(defun coordinate-string-p (s)
  "Predicate to detect valid move strings in
   coordinate notation."
  (let* ((parts (split-move s))
         (len (list-length parts))
         (promotion-chars "rnbq"))
    (and (or (= len 2)
             (= len 3))
         (valid-coord-p (first parts))
         (valid-coord-p (second parts))
         (if (= len 3)
             (find (coerce (third parts) 'character) promotion-chars)
             t))))

(defun san-string-p (s)
  "Predicate to see if given string represents chess move in SAN notation."
  nil)

(defun move-string-p (s)
  "Predicate to see if given string represents a valid chess move."
  (or (coordinate-string-p s)
      (san-string-p s)))

(defun group (list n)
  "Group items in list to lists of n length."
  (when (zerop n) (error "Groups fo zero are no fun"))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                   (rec rest (cons (subseq list 0 n) acc))
                   (nreverse (cons list acc))))))
    (when list (rec list nil))))

(defun list-of (n elt)
  "Returns list of N ELT's."
  (if (zerop n)
      nil
      (cons elt (list-of (1- n) elt))))

(defun expand-digits (x list)
  "Returns new list of characters with each digit character in given
   LIST of characters expanded by that many of X's."
  (labels ((f (result items)
             (cond ((null items) (nreverse result))
                   ((alexandria:when-let ((num (digit-char-p (first items))))
                      (f (append (list-of num x) result)
                         (rest items))))
                   (t (f (cons (first items)  result)
                         (rest items))))))
    (f nil list)))

(defun compact-item (x list)
  "Returns a list where each X in LIST is replaced by digit
   character indicating their amount."
  (labels ((f (result items found)
             (cond ((null items) (if (plusp found)
                                     (nreverse (cons found result))
                                     (nreverse result)))
                   ((equalp (first items) x) (f result (rest items) (1+ found)))
                   (t (if (plusp found)
                          (f (cons (digit-char found) result) items 0)
                          (f (cons (first items) result) (rest items) found))))))
    (f nil list 0)))

(defun interpose (sep list)
  "Returns a list with SEP interposed with given LIST."
  (labels ((f (c)
             (when (not (null c))
               (cons sep (cons (first c)
                               (f (rest c)))))))
    (rest (f list))))

(defun fen->ascii (fen)
  "Return printable string for the board from FEN string.

 8| r n b q k b n r
 7| p p p p p p p p
 6| - - - - - - - -
 5| - - - - - - - -
 4| - - - - - - - -
 3| - - - - - - - -
 2| P P P P P P P P
 1| R N B Q K B N R
 -+----------------
  | a b c d e f g h"
  (with-output-to-string (*STANDARD-OUTPUT*)
    (let ((count 8))
      (dolist (row (group (expand-digits #\- (delete #\/ (concatenate 'list (first (cl-utilities:split-sequence #\space fen))))) 8))
        (format t "~a~%" (concatenate 'string (cons (digit-char count) (cons #\| (interpose #\space row)))))
        (decf count)))))
