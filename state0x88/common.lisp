;;;; common0x88.lisp provides shared variables used by 0x88 board.

(in-package :tursas.state0x88)

;; Type declarations
(deftype board-value () '(signed-byte 8))
(deftype board-vector () '(vector board-value 128))

;; Constants used throughout tursas.state0x88 package
(define-constant +white-king-store+ #x0c)
(define-constant +black-king-store+ #x7c)
(define-constant +turn-store+ #x38)
(define-constant +half-move-store+ #x39)
(define-constant +full-move-n-store+ #x3a)
(define-constant +full-move-store+ #x3b)
(define-constant +castling-store+ #x58)
(define-constant +en-passant-store+ #x59)
(define-constant +dynamic-store+ #x5a)

(define-constant +check-store+ #x5b)

(define-constant +prev-move-from+ #x6a)
(define-constant +prev-move-to+ #x6b)
(define-constant +prev-piece+ #x6c)

(define-constant +white-king-side+ 8)
(define-constant +white-queen-side+ 4)
(define-constant +black-king-side+ 2)
(define-constant +black-queen-side+ 1)

(define-constant +king-side+ 1)
(define-constant +queen-side+ 0)

(define-constant +white+ 0)
(define-constant +black+ 1)

(define-constant +black-queen+ -6)
(define-constant +black-rook+ -5)
(define-constant +black-bishop+ -4)
(define-constant +black-king+ -3)
(define-constant +black-knight+ -2)
(define-constant +black-pawn+ -1)

(define-constant +empty-square+ 0)

(define-constant +white-pawn+ 1)
(define-constant +white-knight+ 2)
(define-constant +white-king+ 3)
(define-constant +white-bishop+ 4)
(define-constant +white-rook+ 5)
(define-constant +white-queen+ 6)

(define-constant +north+ 16)
(define-constant +nn+ 32)
(define-constant +south+ -16)
(define-constant +ss+ -32)
(define-constant +east+ 1)
(define-constant +west+ -1)
(define-constant +ne+ 17)
(define-constant +sw+ -17)
(define-constant +nw+ 15)
(define-constant +se+ -15)

(define-constant +opening-game+ 0)
(define-constant +middle-game+ 1)
(define-constant +end-game+ 2)
