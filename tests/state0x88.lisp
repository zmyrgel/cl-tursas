;; Copyright (c) 2012, Timo Myyrä <timo.myyra@iki.fi>

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

(def-suite state0x88-suite :description "State0x88 package test suite.")
(in-suite state0x88-suite)

;;; BOARD TESTS ;;;
(test board-tests
  (let ((board (State0x88-board (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))))
    (is-false (white-piece-p +black-pawn+))
      (is-false (white-piece-p +black-king+))
      (is-true (white-piece-p +white-pawn+))
      (is-false (white-piece-p +empty-square+))
      (is-true (black-piece-p +black-pawn+))
      (is-false (black-piece-p +white-pawn+))
      (is-false (black-piece-p +empty-square+))

      (is-true (occupied-by-p (board #x10 +white+)))
      (is-true (occupied-by-p (board #x00 +white+)))
      (is-true (occupied-by-p (board #x70 +black+)))
      (is-false (occupied-by-p (board #x70 +white+)))
      (is-false (occupied-by-p (board #x40 +white+)))

      (is (= (opponent +white+) +black+))
      (is (= (opponent +black+) +white+))

      ;; (is (promotionp piece move) )

      (is (piece-name +white-king+) #\K)
      (is (piece-name +black-queen+) #\q)
      (is (piece-name +white-pawn+) #\p)
      (is (piece-name +black-king+) #\k)

      (is (piece-value #\K) +white-king+)
      (is (piece-value #\q) +black-queen+)
      (is (piece-value #\p) +white-pawn+)
      (is (piece-value #\k) +black-king+)

      (is-true (board-index-p #x10))
      (is-true (board-index-p #x0))
      (is-true (board-index-p #x77))
      (is-false (board-index-p #x78))
      (is-false (board-index-p #x128))

      (is-true (empty-square-p board #x20))
      (is-false (empty-square-p board #x00))
      (is-true (empty-square-p board #x57))

      (is (column #x10) 1)
      (is (column #x0) 0)
      (is (column #x60) 6)

      (is (row #x10) 0)
      (is (row #x22) 2)
      (is (row #x42) 2)
      (is (row #x46) 6)

      (is-true (same-column-p #x10 #x20))
      (is-true (same-column-p #x10 #x30))
      (is-false (same-column-p #x20 #x34))

      (is-true (same-row-p #x10 #x50))
      (is-true (same-row-p #x10 #x53))
      (is-false (same-row-p #x11 #x66))

      (is (square-color #x10) +white+)
      (is (square-color #x11) +black+)

      (is-true (same-color-p #x10 #x12))

      (is-true (board-occupied-p board #x10))

      (is (board-ref board #x10) +white-pawn+)
      (is (board-ref board #x30) +empty-square+)

      (is-false (fill-square! board #x45 20))

      (is (king-index board +white+) #x44)))
