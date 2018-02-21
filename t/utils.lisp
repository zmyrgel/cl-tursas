(in-package :cl-user)
(defpackage tursas.t.utils
  (:use :cl :prove))
(in-package :tursas.t.utils)

(plan 23)

;; Test coordinate validation.
(ok (not (valid-coord-p "a2a3")) "checking for invalid coordinate")
(ok (not (valid-coord-p "foo")))
(ok (valid-coord-p "a2"))
(ok (valid-coord-p "h8"))
(ok (valid-coord-p "g3"))
(ok (not (valid-coord-p "k3")))

;; test split move
(is (string= "a2" (multiple-value-bind (from to promotion)
                      (split-move "a2a3")
                    (declare (ignore to promotion))
                    from)))

(ok (not (split-move "foo")))
(ok (not (split-move "foo000")))

;; Test coordinate string representation.
(ok (coordinate-string-p "a2a3"))
(ok (coordinate-string-p "a4h5q"))
(ok (coordinate-string-p "b2c3"))
(ok (coordinate-string-p "h2g1"))
(ok (not (coordinate-string-p "bd32")))
(ok (not (coordinate-string-p "h7h8g")))

;; dummy SAN test
(ok (not (san-string-p "foo")))

;; move-string tests
(ok (move-string-p "a2a3"))
(ok (move-string-p "h6h8b"))

;; expand-digits test
(is (expand-digits #\x '(#\a #\3 #\b #\2 #\b))
    '(#\a #\x #\x #\x #\b #\x #\x #\b))
(is-error (expand-digits #\x '(#\a #\3 #\b 2 #\b)) 'type-error)

;; compact-item test
(is (compact-item #\x '(#\a #\x #\x #\x #\b))
    '(#\a #\3 #\b))

(is (string-indexed "foo")
      '((0 #\f) (1 #\o) (2 #\o)))

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
                   "| a b c d e f g h"))

(finalize)
