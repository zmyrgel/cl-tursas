;; Copyright (c) 2011, Timo Myyrä <timo.myyra@gmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tursas.tests)

(in-suite utils-suite)

;; (def-fixture person-schema []
;;   (with-connection hdb
;;     (try
;;      (do-commands "create table person (name varchar(255), age integer)")
;;      (insert-values :person [:name] ["bill"] ["joey"])
;;      (with-query-results results [query]
;;        (test-body))
;;      (finally
;;       (do-commands "drop schema public cascade")))))

;; (deftest test-ideal
;;   (with-fixture person-schema []
;;     (is (= "bill" (:name (first results))))
;;     (is (= "joey" (:name (second results)))))))


(test valid-coord-test
  "Test coordinate validation."
  (is (null (valid-coord-p "a2a3")))
  (is (null (valid-coord-p "foo")))
  (is (eq t (valid-coord-p "a2")))
  (is (eq t (valid-coord-p "h8")))
  (is (eq t (valid-coord-p "g3")))
  (is (null (valid-coord-p "k3"))))

(test split-move-test
      "Test move splitting."
      (is (string= "a2" (multiple-value-bind (from to promotion)
                            (split-move "a2a3")
                          from)))
      (is (null (split-move "foo")))
      (is (null (split-move "foo000"))))

(test coordinate-string-test
      "Test coordinate string representation."
      (is (eq t (coordinate-string-p "a2a3")))
      (is (eq t (coordinate-string-p "a4h5q")))
      (is (eq t (coordinate-string-p "b2c3")))
      (is (eq t (coordinate-string-p "h2g1")))
      (is (eq nil (coordinate-string-p "bd32")))
      (is (eq nil (coordinate-string-p "h7h8g"))))

(test san-string-test
      "Dummy test"
      (is (eq nil (san-string-p "foo"))))

(test move-string-test
      "Testing stuff."
      (is (eq t (move-string-p "a2a3")))
      (is (eq t (move-string-p "h6h8b"))))

;; XXX: not exported, don't test
;; (test group-test
;;       "Test group function"
;;       (is (equal (group '(a b c d) 2)
;;                  '((a b) (c d)))))

;; (test list-of-test
;;       "Test list-of function"
;;       (is (equal (list-of 3 'a)
;;                  '(a a a))))

(test expand-digits-test
      "Test digit expansion."
      (is (expand-digits #\x '(#\a #\3 #\b #\2 #\b))
          '(#\a #\x #\x #\x #\b #\x #\x #\b))
      (is (expand-digits #\x '(#\a #\3 #\b 2 #\b))
          '(#\a #\x #\x #\x #\b 2 #\b)))

(test compact-item-test
      "Test compacting."
      (is (compact-item #\x '(#\a #\x #\x #\x #\b))
          '(#\a #\3 #\b)))

(test str-test
  "Test string creation"
  (is (string= (str "foo" "bar" "baz")
               "foobarbaz")))

;; XXX: not exported, don't test
;; (test interpose-test
;;       "Test interposing"
;;       (is (interpose #\space '(#\a #\b #\c))
;;           '(#\a #\space #\b #\space #\c)))

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
