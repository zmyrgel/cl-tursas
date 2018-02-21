;;;; this file contains the generic utils tests in tursas.
(in-package :tursas.tests)

(5am:def-suite utils-suite :description "Utils package test suite.")
(5am:in-suite utils-suite)

(test valid-coord-test
  "Test coordinate validation."
  (5am:is-false (valid-coord-p "a2a3"))
  (is-false (valid-coord-p "foo"))
  (is-true (valid-coord-p "a2"))
  (is-true (valid-coord-p "h8"))
  (is-true (valid-coord-p "g3"))
  (is-false (valid-coord-p "k3")))

(5am:test split-move-test
  "Test move splitting."
  (is (string= "a2" (multiple-value-bind (from to promotion)
                        (split-move "a2a3")
                      (declare (ignore to promotion))
                      from)))
  (is-false (split-move "foo"))
  (is-false (split-move "foo000")))

(test coordinate-string-test
  "Test coordinate string representation."
  (is-true (coordinate-string-p "a2a3"))
  (is-true (coordinate-string-p "a4h5q"))
  (is-true (coordinate-string-p "b2c3"))
  (is-true (coordinate-string-p "h2g1"))
  (is-false (coordinate-string-p "bd32"))
  (is-false (coordinate-string-p "h7h8g")))

(test san-string-test
  "Dummy test"
  (is-false (san-string-p "foo")))

(test move-string-test
  "Testing stuff."
  (is-true (move-string-p "a2a3"))
  (is-true (move-string-p "h6h8b")))

(test expand-digits-test
  "Test digit expansion."
  (is (expand-digits #\x '(#\a #\3 #\b #\2 #\b))
      '(#\a #\x #\x #\x #\b #\x #\x #\b))
  (signals type-error (expand-digits #\x '(#\a #\3 #\b 2 #\b))))

(test compact-item-test
  "Test compacting."
  (is (compact-item #\x '(#\a #\x #\x #\x #\b))
      '(#\a #\3 #\b)))

(test str-test
  "Test string creation"
  (is-true (string= (str "foo" "bar" "baz")
                    "foobarbaz")))

(test string-indexed-test
  "Test string indexing"
  (is (string-indexed "foo")
      '((0 #\f) (1 #\o) (2 #\o))))

(test fen->ascii-test
  "Test ascii printing."
  (is (fen->ascii "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
      (concatenate 'string
                   "8| r n b q k b n r\n"
                   "7| p p p p p p p p\n"
                   "6| - - - - - - - -\n"
                   "5| - - - - - - - -\n"
                   "4| - - - - - - - -\n"
                   "3| - - - - - - - -\n"
                   "2| P P P P P P P P\n"
                   "1| R N B Q K B N R\n"
                   "-+----------------\n"
                   "| a b c d e f g h")))
