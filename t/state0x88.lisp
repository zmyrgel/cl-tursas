(in-package :cl-user)
(defpackage tursas.t.state0x88
  (:use :cl :prove))
(in-package :tursas.t.state0x88)

(plan 56)

;;; FEN conversion tests ;;;
;; test fen->state
;; test state->fen

;; move generation tests
;; given known positions, calculate correct moves
;; add bunch of known difficult cases so those get tested, castling while square threatened, en-passants with check etc.

;;; MOVE TESTS
(let ((state (tursas.state0x88::fen->state "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")))
  (ok (member "e1g1" (mapcar #'tursas.state0x88::move->coord
                             (tursas.state0x88::legal-moves state))
              :test 'string=)
      "e1g1 castling move should be valid move to make."))

;; test that check is detected and is fixed by protecting king.
;; XXX: broken test
;; (let ((state (tursas.state0x88::fen->state "r1backbone/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")))
;;   (ok (tursas.state0x88::checkp state))
;;   (ok (not (tursas.state0x88::checkp (tursas.state0x88::apply-move state (tursas.state0x88::coord->move "d8d7"))))))

(let ((state (tursas.state0x88::fen->state "3brr2/7b/8/2pN3Q/2p2k2/5P2/4P1KR/2N2RB1 b - - 1 18"))
      (state2 (tursas.state0x88::fen->state "3brr2/2N4b/8/2p4Q/2p2k2/5P2/4P1KR/2N2RB1 w - - 1 17")))
  (ok (tursas.state0x88::matep state)
      "there shouldn't be valid moves left in state, ensure mate is detected")
  ;; Why following is here? isn't it normal state?
  ;; (ok (tursas.state0x88::matep state2)
  ;;     "there shouldn't be valid moves left in state, ensure mate is detected")
  )

;; test that white can promote
(let ((state (tursas.state0x88::fen->state "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")))
  (ok
   (member "c7c8Q" (mapcar #'tursas.state0x88::move->coord
                           (tursas.state0x88::legal-moves state))
           :test 'string=)
   "c7c8Q should be valid promotion move to make"))

(let ((state (tursas.state0x88::fen->state "rnbqkbn1/pPpp3r/4pppp/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6")))
  (ok
   (member "b7c8" (mapcar #'tursas.state0x88::move->coord
                          (tursas.state0x88::legal-moves state))
           :test 'string=)
   "Pawn promotion should be listed as allowed move.")
  (ok
   (member "b7a8" (mapcar #'tursas.state0x88::move->coord
                          (tursas.state0x88::legal-moves state))
           :test 'string=)
   "Pawn promotion should be listed as allowed move."))

;; (test en-passant-tests
;;       (let ((state (fen->state "rnbqkb1r/pppppppp/7n/P7/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2")))

;;     ))

;;; BOARD TESTS ;;;
(let ((board (tursas.state0x88::State0x88-board (tursas.state0x88::fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))))
  (ok (not (tursas.state0x88::white-piece-p tursas.state0x88::+black-pawn+))
      "black pawn should not be detected as white")
  (ok (not (tursas.state0x88::white-piece-p tursas.state0x88::+black-king+))
      "black king should not be detected as white")
  (ok (tursas.state0x88::white-piece-p tursas.state0x88::+white-pawn+)
      "white pawn should be detected as white")
  (ok (not (tursas.state0x88::white-piece-p tursas.state0x88::+empty-square+))
      "empty square should not be detected as white")
  (ok (tursas.state0x88::black-piece-p tursas.state0x88::+black-pawn+)
      "black pawn should be detected as black")
  (ok (not (tursas.state0x88::black-piece-p tursas.state0x88::+white-pawn+))
      "white pawn should not be detected as black")
  (ok (not (tursas.state0x88::black-piece-p tursas.state0x88::+empty-square+))
      "empty square should not be detected as black")

  (ok (tursas.state0x88::occupied-by-p board #x10 tursas.state0x88::+white+)
      "white pawn rank should be occupied by white in start position")
  (ok (tursas.state0x88::occupied-by-p board #x00 tursas.state0x88::+white+)
      "first rank should be occupied by white pieces")
  (ok (tursas.state0x88::occupied-by-p board #x70 tursas.state0x88::+black+)
      "last rank should be occupied by black pieces")
  (ok (not (tursas.state0x88::occupied-by-p board #x70 tursas.state0x88::+white+))
      "last rank should not have white piece")
  (ok (not (tursas.state0x88::occupied-by-p board #x40 tursas.state0x88::+white+))
      "middle of the board should not have white piece on empty square")

  (is (tursas.state0x88::opponent tursas.state0x88::+white+) tursas.state0x88::+black+
      "white players opponent should be black")
  (is (tursas.state0x88::opponent tursas.state0x88::+black+) tursas.state0x88::+white+
      "black players opponent should be white")

    ;;(is (promotionp piece move) )

  (is (tursas.state0x88::piece-name tursas.state0x88::+white-king+) #\K
      "white kings piece name should be K")
  (is (tursas.state0x88::piece-name tursas.state0x88::+black-queen+) #\q
      "black queens piece name should be q")
  (is (tursas.state0x88::piece-name tursas.state0x88::+white-pawn+) #\P
      "white pawns piece name should be P")
  (is (tursas.state0x88::piece-name tursas.state0x88::+black-king+) #\k
      "black kings piece name should be k")

  (is (tursas.state0x88::piece-value #\K) tursas.state0x88::+white-king+
      "piece value of K should be equal to white kings value")
  (is (tursas.state0x88::piece-value #\q) tursas.state0x88::+black-queen+
      "piece value of Q should be equal to black queens value")
  (is (tursas.state0x88::piece-value #\P) tursas.state0x88::+white-pawn+
      "piece value of P should be equal to white pawn")
  (is (tursas.state0x88::piece-value #\k) tursas.state0x88::+black-king+
      "piece value of k should be equal to black kings value")

  (ok (tursas.state0x88::board-index-p #x10)
      "0x10 should be board index")
  (ok (tursas.state0x88::board-index-p #x0)
      "0x0 should be board index")
  (ok (tursas.state0x88::board-index-p #x77)
      "0x77 should be board index")
  (ok (not (tursas.state0x88::board-index-p #x78))
      "0x78 should not be board index, outside board indexes but within the array")
  (ok (not (tursas.state0x88::board-index-p #x128))
      "0x128 should not be board index, outside of 0x88 array")

  (ok (tursas.state0x88::empty-square-p board #x20)
      "0x20 index should be empty on start position")
  (ok (not (tursas.state0x88::empty-square-p board #x00))
      "0x00 should not be empty on start position")
  (ok (tursas.state0x88::empty-square-p board #x57)
      "0x57 should be empty square on start position")

  (is (tursas.state0x88::rank #x10) 1
      "index 0x10 should be on second rank")
  (is (tursas.state0x88::rank #x33) 3
      "index 0x22 should be on rank 4")
  (is (tursas.state0x88::rank #x42) 4
      "index 0x42 should be on rank 2")
  (is (tursas.state0x88::rank #x46) 4
      "index 0x46 should be on rank 7")

  (is (tursas.state0x88::file #x10) 0
      "0x10 should be on first file")
  (is (tursas.state0x88::file #x34) 4
      "0x33 should be on fourth file")
  (is (tursas.state0x88::file #x57) 7
      "0x57 should be on eigth file")

  (ok (tursas.state0x88::same-file-p #x10 #x20)
      "indexes 0x10 and 0x20 should on same file")
  (ok (tursas.state0x88::same-file-p #x10 #x30)
      "indexes 0x10 and 0x30 should be on same file")
  (ok (not (tursas.state0x88::same-file-p #x20 #x34))
      "indexes 0x20 and 0x34 should on different file")

  (ok (not (tursas.state0x88::same-rank-p #x10 #x50))
      "index 0x10 and 0x50 should not be on same rank")
  (ok (not (tursas.state0x88::same-rank-p #x10 #x53))
      "indexes 0x10 and 0x53 should not be on same rank")
  (ok (tursas.state0x88::same-rank-p #x40 #x46)
      "indexes 0x40 and 0x46 should be on same rank")

  (is (tursas.state0x88::square-color #x10) tursas.state0x88::+white+
      "square color of index 0x10 should be white")
  (is (tursas.state0x88::square-color #x11) tursas.state0x88::+black+
      "square color of index 0x11 should be black")

  (ok (tursas.state0x88::same-color-p #x10 #x12)
      "indexes 0x10 and 0x12 should be same color")

  (ok (tursas.state0x88::board-occupied-p board #x10)
      "index 0x10 should be occupied on start position")

  (is (tursas.state0x88::board-ref board #x10) tursas.state0x88::+white-pawn+
      "index 0x10 should contain white pawn in start position")
  (is (tursas.state0x88::board-ref board #x30) tursas.state0x88::+empty-square+
      "index 0x30 should be empty on start position")

  (ok (not (tursas.state0x88::fill-square! board #x45 20))
      "filling square 0x45 should not succeed... for reasons I can't think of currently")

  (is (tursas.state0x88::king-index board tursas.state0x88::+white+) #x04
      "white king should be on board index 0x44 on start position")
  )

(finalize)
