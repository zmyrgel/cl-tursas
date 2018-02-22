(in-package :cl-user)
(defpackage tursas.t.utils
  (:use :cl :prove
        :tursas.utils))
(in-package :tursas.t.utils)

(plan 27)

;; Test coordinate validation.
(ok (not (valid-coord-p "a2a3"))
    "invalid coordinates should return nil")
(ok (not (valid-coord-p "foo"))
    "invalid coordinate should return nil")
(ok (valid-coord-p "a2")
    "short coordinates should return t")
(ok (valid-coord-p "h8")
    "short coordinates should return t")
(ok (valid-coord-p "g3")
    "short coordinates should return t")
(ok (not (valid-coord-p "k3"))
    "short coordinates outside board should not be valid")

;; test split move
(let ((coordinate "a2a3"))
  (is "a2" (multiple-value-bind (from to promotion)
             (split-move coordinate)
           (declare (ignore to promotion))
             from)
      "split move should assign from-field properly")
  (is "a3" (multiple-value-bind (from to promotion)
             (split-move coordinate)
           (declare (ignore from promotion))
             to)
      "split move should assign to-field properly")
  (is "" (multiple-value-bind (from to promotion)
             (split-move coordinate)
           (declare (ignore from to))
           promotion)
      "split move should assign empty string to promotion if no promotion char is given"))

(ok (not (split-move "foo"))
    "split-move with too short string should return nil")
(ok (not (split-move "foo000"))
    "split-move with too long string should return nil")

;; Test coordinate string representation.
(ok (coordinate-string-p "a2a3")
    "coordinate-string-p should work with proper coordinate")
(ok (coordinate-string-p "a4h5q")
    "coordinate-string-p should work with proper promotion coordinate")
(ok (coordinate-string-p "b2c3")
    "coordinate-string-p should work with proper coordinate")
(ok (coordinate-string-p "h2g1")
    "coordinate-string-p should work with proper coordinate")
(ok (not (coordinate-string-p "bd32"))
    "coordinate-string-p should not work with improper coordinate")
(ok (not (coordinate-string-p "h7h8g"))
    "coordinate-string-p should not work with promotion to invalid piece")

;; XXX: function won't work
;; (ok (not (san-string-p "foo"))
;;     "dummy SAN string should return nil")

;; move-string tests
(ok (move-string-p "a2a3")
    "valid move string should work with move-string-p")
(ok (move-string-p "h6h8b")
    "valid promotion move string should be t")
(ok (not (move-string-p "h6h8x"))
    "invalid promotion piece in move string should fail")
(ok (not (move-string-p "i6h8"))
    "invalid from coordinate in move string should fail")
(ok (not (move-string-p "h6i8"))
    "invalid to coordinate in move string should fail")

(is (expand-digits #\x '(#\a #\3 #\b #\2 #\b))
    '(#\a #\x #\x #\x #\b #\x #\x #\b)
    "expand-digits should work ok when given proper arguments")
(is-error (expand-digits #\x '(#\a #\3 #\b 2 #\b)) 'type-error
          "expand-digits should return error if digits are not given as characters")

(is (compact-item #\x '(#\a #\x #\x #\x #\b))
    '(#\a #\3 #\b)
    "compact item should shorten list of characters")

(is (string-indexed "foo")
    '((0 . #\f) (1 . #\o) (2 . #\o))
    "string-indexed should generate proper alist with index character pairs")

(is (fen->ascii "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    (format nil "8|r n b q k b n r~%7|p p p p p p p p~%6|- - - - - - - -~%5|- - - - - - - -~%4|- - - - - - - -~%3|- - - - - - - -~%2|P P P P P P P P~%1|R N B Q K B N R~%-+---------------~%| a b c d e f g h~%")
    "start position fen string generate proper board representation.")

(finalize)
