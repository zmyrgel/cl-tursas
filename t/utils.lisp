(in-package :cl-user)
(defpackage tursas.t.utils
  (:use :cl :rove
        :tursas.utils))
(in-package :tursas.t.utils)

(deftest coordinate-tests
  (testing "coordinate validation"
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
        "short coordinates outside board should not be valid")))

;; test split move
(deftest split-move-tests
  (testing "splitty"
    (let ((coordinate "a2a3"))
      (ok (equalp "a2" (multiple-value-bind (from to promotion)
                           (split-move coordinate)
                         (declare (ignore to promotion))
                         from))
          "split move should assign from-field properly")
      (ok (equalp "a3" (multiple-value-bind (from to promotion)
                           (split-move coordinate)
                         (declare (ignore from promotion))
                         to))
          "split move should assign to-field properly")
      (ok (equalp "" (multiple-value-bind (from to promotion)
                         (split-move coordinate)
                       (declare (ignore from to))
                       promotion))
          "split move should assign empty string to promotion if no promotion char is given"))

    (ng (split-move "foo")
        "split-move with too short string should return nil")
    (ng (split-move "foo000")
        "split-move with too long string should return nil")))

(deftest coordinate-string-tests
  (testing "coordinate string representation tests"
    (ok (coordinate-string-p "a2a3")
        "coordinate-string-p should work with proper coordinate")
    (ok (coordinate-string-p "a4h5q")
        "coordinate-string-p should work with proper promotion coordinate")
    (ok (coordinate-string-p "b2c3")
        "coordinate-string-p should work with proper coordinate")
    (ok (coordinate-string-p "h2g1")
        "coordinate-string-p should work with proper coordinate")
    (ng (coordinate-string-p "bd32")
        "coordinate-string-p should not work with improper coordinate")
    (ng (coordinate-string-p "h7h8g")
        "coordinate-string-p should not work with promotion to invalid piece")))

;; XXX: function won't work
(deftest san-string-tests
  (testing "SAN string tests"
    (ng (san-string-p "foo")
        "dummy SAN string should return nil")))

;; move-string tests
(deftest move-string-tests
  (testing "move string detection"
    (ok (move-string-p "a2a3")
        "valid move string should work with move-string-p")
    (ok (move-string-p "h6h8b")
        "valid promotion move string should be t")
    (ng (move-string-p "h6h8x")
        "invalid promotion piece in move string should fail")
    (ng (move-string-p "i6h8")
        "invalid from coordinate in move string should fail")
    (ng (move-string-p "h6i8")
        "invalid to coordinate in move string should fail")))

(deftest expand-digits-tests
  (testing "testing that digit expansion works"
    (ok (equalp (expand-digits #\x '(#\a #\3 #\b #\2 #\b))
                '(#\a #\x #\x #\x #\b #\x #\x #\b))
        "expand-digits should work ok when given proper arguments")
    (ok (signals (expand-digits #\x '(#\a #\3 #\b 2 #\b)) 'type-error)
        "expand-digits should return error if digits are not given as characters")))


(deftest compact-item-tests
  (testing "testing that item compaction works"
    (ok (equalp (compact-item #\x '(#\a #\x #\x #\x #\b))
                '(#\a #\3 #\b))
        "compact item should shorten list of characters")))

(deftest string-indexed-tests
  (testing "string indexing"
    (ok (equalp (string-indexed "foo")
                '((0 . #\f) (1 . #\o) (2 . #\o)))
        "string-indexed should generate proper alist with index character pairs")))

(deftest fen-transformation-tests
  (testing "fen to string transformation"
    (ok (equalp
         (fen->ascii "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (format nil "8| r n b q k b n r~%7| p p p p p p p p~%6| - - - - - - - -~%5| - - - - - - - -~%4| - - - - - - - -~%3| - - - - - - - -~%2| P P P P P P P P~%1| R N B Q K B N R~%-+----------------~% | a b c d e f g h~%"))
        "start position fen string generate proper board representation.")))
