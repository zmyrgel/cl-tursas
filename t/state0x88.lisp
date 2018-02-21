(in-package :cl-user)
(defpackage tursas.t.state0x88
  (:use :cl :prove))
(in-package :tursas.t.state0x88)

;;; FEN conversion tests ;;;
;; test fen->state
;; test state->fen

;; move generation tests
;; given known positions, calculate correct moves

;;; MOVE TESTS
(let ((state (tursas.state0x88::fen->state "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")))
  (ok (member "e1g1" (mapcar #'move->coord
                             (legal-moves state))
              :test 'string=))
  "test that white can make normal castling move")

;; test that check is detected and is fixed by protecting king.
(let ((state (fen->state "r1backbone/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")))
  (ok (checkp state))
  (ok (not (checkp (apply-move state (coord->move "d8d7"))))))

(let ((state (fen->state "3brr2/7b/8/2pN3Q/2p2k2/5P2/4P1KR/2N2RB1 b - - 1 18"))
      (state2 (fen->state "3brr2/2N4b/8/2p4Q/2p2k2/5P2/4P1KR/2N2RB1 w - - 1 17")))
  (ok (matep state))
  (ok (matep state2)))

;; test that white can promote
(let ((state (fen->state "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")))
  (ok
   (member "c7c8Q" (mapcar #'move->coord
                           (legal-moves state))
           :test 'string=)))

;; (test en-passant-tests
;;       (let ((state (fen->state "rnbqkb1r/pppppppp/7n/P7/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2")))

;;     ))

;;; BOARD TESTS ;;;
(let ((board (State0x88-board (tursas.state0x88::fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))))
    (ok (not (tursas.state0x88::white-piece-p tursas.state0x88::+black-pawn+)))
    (ok (not (tursas.state0x88::white-piece-p tursas.state0x88::+black-king+)))
    (ok (tursas.state0x88::white-piece-p tursas.state0x88::+white-pawn+))
    (ok (not (tursas.state0x88::white-piece-p tursas.state0x88::+empty-square+)))
    (ok (tursas.state0x88::black-piece-p tursas.state0x88::+black-pawn+))
    (ok (not (tursas.state0x88::black-piece-p tursas.state0x88::+white-pawn+)))
    (ok (not (tursas.state0x88::black-piece-p tursas.state0x88::+empty-square+)))

    (ok (tursas.state0x88::occupied-by-p (board #x10 tursas.state0x88::+white+)))
    (ok (tursas.state0x88::occupied-by-p (board #x00 tursas.state0x88::+white+)))
    (ok (tursas.state0x88::occupied-by-p (board #x70 tursas.state0x88::+black+)))
    (ok (not (tursas.state0x88::occupied-by-p (board #x70 tursas.state0x88::+white+))))
    (ok (not (tursas.state0x88::occupied-by-p (board #x40 tursas.state0x88::+white+))))

    (is (= (tursas.state0x88::opponent tursas.state0x88::+white+) tursas.state0x88::+black+))
    (is (= (tursas.state0x88::opponent tursas.state0x88::+black+) tursas.state0x88::+white+))

    ;; (is (promotionp piece move) )

    (is (tursas.state0x88::piece-name tursas.state0x88::+white-king+) #\K)
    (is (tursas.state0x88::piece-name tursas.state0x88::+black-queen+) #\q)
    (is (tursas.state0x88::piece-name tursas.state0x88::+white-pawn+) #\p)
    (is (tursas.state0x88::piece-name tursas.state0x88::+black-king+) #\k)

    (is (tursas.state0x88::piece-value #\K) tursas.state0x88::+white-king+)
    (is (tursas.state0x88::piece-value #\q) tursas.state0x88::+black-queen+)
    (is (tursas.state0x88::piece-value #\p) tursas.state0x88::+white-pawn+)
    (is (tursas.state0x88::piece-value #\k) tursas.state0x88::+black-king+)

    (ok (tursas.state0x88::board-index-p #x10))
    (ok (tursas.state0x88::board-index-p #x0))
    (ok (tursas.state0x88::board-index-p #x77))
    (ok (not (tursas.state0x88::board-index-p #x78)))
    (ok (not (tursas.state0x88::board-index-p #x128)))

    (ok (tursas.state0x88::empty-square-p board #x20))
    (ok (not (tursas.state0x88::empty-square-p board #x00)))
    (ok (tursas.state0x88::empty-square-p board #x57))

    (is (tursas.state0x88::column #x10) 1)
    (is (tursas.state0x88::column #x0) 0)
    (is (tursas.state0x88::column #x60) 6)

    (is (tursas.state0x88::row #x10) 0)
    (is (tursas.state0x88::row #x22) 2)
    (is (tursas.state0x88::row #x42) 2)
    (is (tursas.state0x88::row #x46) 6)

    (ok (tursas.state0x88::same-column-p #x10 #x20))
    (ok (tursas.state0x88::same-column-p #x10 #x30))
    (ok (not (tursas.state0x88::same-column-p #x20 #x34)))

    (ok (tursas.state0x88::same-row-p #x10 #x50))
    (ok (tursas.state0x88::same-row-p #x10 #x53))
    (ok (not (tursas.state0x88::same-row-p #x11 #x66)))

    (is (tursas.state0x88::square-color #x10) tursas.state0x88::+white+)
    (is (tursas.state0x88::square-color #x11) tursas.state0x88::+black+)

    (ok (tursas.state0x88::same-color-p #x10 #x12))

    (ok (tursas.state0x88::board-occupied-p board #x10))

    (is (tursas.state0x88::board-ref board #x10) tursas.state0x88::+white-pawn+)
    (is (tursas.state0x88::board-ref board #x30) tursas.state0x88::+empty-square+)

    (ok (not (tursas.state0x88::fill-square! board #x45 20)))

    (is (tursas.state0x88::king-index board tursas.state0x88::+white+) #x44))
