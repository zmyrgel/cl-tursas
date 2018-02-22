(in-package :cl-user)
(defpackage tursas.utils
  (:documentation "provide generic utilities used by tursas chess engine. Utilities are not specific to any chess board representation.")
  (:use :cl)
  (:import-from :alexandria :define-constant :when-let)
  (:import-from :cl-utilities :split-sequence)
  (:export :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :string-indexed))

(in-package :tursas.utils)

(define-constant valid-coords
    (loop for x across "abcdefgh"
          nconc (loop for y below 8 collect (format nil "~a~a" x (1+ y))))
  :test 'equal
  :documentation "List of valid chess coordinate strings.")

(defun valid-coord-p (s)
  "Checks if given string s is a valid chess board coordinate."
  (some (lambda (coord)
          (equalp coord s)) valid-coords))

(defun split-move (algebraic)
  "Partitions chess move given in coordinate notation to pair of coordinates
   and possible promotion character."
  (values (subseq algebraic 0 2)
          (subseq algebraic 2 4)
          (subseq algebraic 4)))

(defun coordinate-string-p (s)
  "Predicate to detect valid move strings in
   coordinate notation."
  (multiple-value-bind (from to promotion)
      (split-move s)
    (and (valid-coord-p from)
         (valid-coord-p to)
         (or (string= promotion "")
             (find (coerce promotion 'character) "rnbq")))))

(defun san-string-p (s)
  "Predicate to see if given string represents chess move in SAN notation."
  (declare (ignore s))
  nil)

(defun move-string-p (s)
  "Predicate to see if given string S represents a valid and supported chess move."
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

(defun expand-digits (x list)
  "Returns new list of characters with each digit character in given
   LIST of characters expanded by that many of X's."
  (labels ((f (result items)
             (cond ((null items) (nreverse result))
                   ((when-let ((num (digit-char-p (first items))))
                      (f (append (make-list num :initial-element x) result)
                         (rest items))))
                   (t (f (cons (first items)  result)
                         (rest items))))))
    (f nil list)))

(defun compact-item (x list)
  "Returns a list where each X in LIST is replaced by digit
   character indicating their amount."
  (labels ((f (result items found)
             (cond ((null items)
                    (if (plusp found)
                        (nreverse (cons (digit-char found) result))
                        (nreverse result)))
                   ((equalp (first items) x)
                    (f result (rest items) (1+ found)))
                   ((plusp found)
                    (f (cons (digit-char found) result) items 0))
                   (t
                    (f (cons (first items) result) (rest items) found)))))
    (f nil list 0)))

(defun interpose (sep list)
  "Returns a list with SEP interposed with given LIST."
  (rest (loop for item in list
              nconc (list sep item))))

(defun string-indexed (s)
  "Return an alist of offset / character pairs for string S."
  (loop for i across s
        for n from 0
        collect (cons n i)))

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
  (with-output-to-string (s)
    (loop for row in (group (expand-digits #\- (delete #\/ (concatenate 'list (first (split-sequence #\space fen))))) 8)
          for n from 8 downto 1
          do (format s "~a~%" (concatenate 'string (list* (digit-char n) #\| (interpose #\space row)))))
    (format s "~{~a~%~}" (list  "-+----------------"  "| a b c d e f g h"))))
