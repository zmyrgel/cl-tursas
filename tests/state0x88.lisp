(in-package :tursas.tests)

(def-suite state0x88-suite :description "State0x88 package test suite.")
(in-suite state0x88-suite)

;;; FEN conversion tests ;;;
;; test fen->state
;; test state->fen

;; move generation tests
;; given known positions, calculate correct moves

;;; MOVE TESTS ;;;

;; test that white can make normal castling move
(test castling-move-tests
  (let ((state (tursas.state0x88::fen->state "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")))
    (is-true
     (member "e1g1" (mapcar #'move->coord
                            (legal-moves state))
             :test 'string=))))

;; test that check is detected and is fixed by protecting king.
(test check-moves-tests
  (let ((state (fen->state "r1backbone/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")))
    (is-true (checkp state))
    (is-false (checkp (apply-move state (coord->move "d8d7"))))))

(test mate-moves-tests
  (let ((state (fen->state "3brr2/7b/8/2pN3Q/2p2k2/5P2/4P1KR/2N2RB1 b - - 1 18"))
        (state2 (fen->state "3brr2/2N4b/8/2p4Q/2p2k2/5P2/4P1KR/2N2RB1 w - - 1 17")))
    (is-true (matep state))
    (is-true (matep state2))))

;; test that white can promote
(test promotion-tests
  (let ((state (fen->state "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")))
    (is-true
     (member "c7c8Q" (mapcar #'move->coord
                            (legal-moves state))
             :test 'string=))))

;; (test en-passant-tests
;;       (let ((state (fen->state "rnbqkb1r/pppppppp/7n/P7/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2")))

;;     ))

;;; BOARD TESTS ;;;
(test board-tests
  (let ((board (State0x88-board (tursas.state0x88::fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))))
    (is-false (tursas.state0x88::white-piece-p tursas.state0x88::+black-pawn+))
    (is-false (tursas.state0x88::white-piece-p tursas.state0x88::+black-king+))
    (is-true (tursas.state0x88::white-piece-p tursas.state0x88::+white-pawn+))
    (is-false (tursas.state0x88::white-piece-p tursas.state0x88::+empty-square+))
    (is-true (tursas.state0x88::black-piece-p tursas.state0x88::+black-pawn+))
    (is-false (tursas.state0x88::black-piece-p tursas.state0x88::+white-pawn+))
    (is-false (tursas.state0x88::black-piece-p tursas.state0x88::+empty-square+))

    (is-true (tursas.state0x88::occupied-by-p (board #x10 tursas.state0x88::+white+)))
    (is-true (tursas.state0x88::occupied-by-p (board #x00 tursas.state0x88::+white+)))
    (is-true (tursas.state0x88::occupied-by-p (board #x70 tursas.state0x88::+black+)))
    (is-false (tursas.state0x88::occupied-by-p (board #x70 tursas.state0x88::+white+)))
    (is-false (tursas.state0x88::occupied-by-p (board #x40 tursas.state0x88::+white+)))

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

    (is-true (tursas.state0x88::board-index-p #x10))
    (is-true (tursas.state0x88::board-index-p #x0))
    (is-true (tursas.state0x88::board-index-p #x77))
    (is-false (tursas.state0x88::board-index-p #x78))
    (is-false (tursas.state0x88::board-index-p #x128))

    (is-true (tursas.state0x88::empty-square-p board #x20))
    (is-false (tursas.state0x88::empty-square-p board #x00))
    (is-true (tursas.state0x88::empty-square-p board #x57))

    (is (tursas.state0x88::column #x10) 1)
    (is (tursas.state0x88::column #x0) 0)
    (is (tursas.state0x88::column #x60) 6)

    (is (tursas.state0x88::row #x10) 0)
    (is (tursas.state0x88::row #x22) 2)
    (is (tursas.state0x88::row #x42) 2)
    (is (tursas.state0x88::row #x46) 6)

    (is-true (tursas.state0x88::same-column-p #x10 #x20))
    (is-true (tursas.state0x88::same-column-p #x10 #x30))
    (is-false (tursas.state0x88::same-column-p #x20 #x34))

    (is-true (tursas.state0x88::same-row-p #x10 #x50))
    (is-true (tursas.state0x88::same-row-p #x10 #x53))
    (is-false (tursas.state0x88::same-row-p #x11 #x66))

    (is (tursas.state0x88::square-color #x10) tursas.state0x88::+white+)
    (is (tursas.state0x88::square-color #x11) tursas.state0x88::+black+)

    (is-true (tursas.state0x88::same-color-p #x10 #x12))

    (is-true (tursas.state0x88::board-occupied-p board #x10))

    (is (tursas.state0x88::board-ref board #x10) tursas.state0x88::+white-pawn+)
    (is (tursas.state0x88::board-ref board #x30) tursas.state0x88::+empty-square+)

    (is-false (tursas.state0x88::fill-square! board #x45 20))

    (is (tursas.state0x88::king-index board tursas.state0x88::+white+) #x44)))
