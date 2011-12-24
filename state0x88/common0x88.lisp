(in-package :tursas.state0x88)

(defconstant +white-king-store+ #x0c)
(defconstant +black-king-store+ #x7c)
(defconstant +turn-store+ #x38)
(defconstant +half-move-store+ #x39)
(defconstant +full-move-n-store+ #x3a)
(defconstant +full-move-store+ #x3b)
(defconstant +castling-store+ #x58)
(defconstant +en-passant-store+ #x59)
(defconstant +dynamic-store+ #x5a)

(defconstant +check-store+ #x5b)

(defconstant +prev-move-from+ #x6a)
(defconstant +prev-move-to+ #x6b)
(defconstant +prev-piece+ #x6c)

(defconstant +white-king-side+ 8)
(defconstant +white-queen-side+ 4)
(defconstant +black-king-side+ 2)
(defconstant +black-queen-side+ 1)

(defconstant +king-side+ 1)
(defconstant +queen-side+ 0)

(defconstant +white+ 0)
(defconstant +black+ 1)

(defconstant +black-queen+ -6)
(defconstant +black-rook+ -5)
(defconstant +black-bishop+ -4)
(defconstant +black-king+ -3)
(defconstant +black-knight+ -2)
(defconstant +black-pawn+ -1)

(defconstant +empty-square+ 0)

(defconstant +white-pawn+ 1)
(defconstant +white-knight+ 2)
(defconstant +white-king+ 3)
(defconstant +white-bishop+ 4)
(defconstant +white-rook+ 5)
(defconstant +white-queen+ 6)

(defconstant +north+ 16)
(defconstant +nn+ 32)
(defconstant +south+ -16)
(defconstant +ss+ -32)
(defconstant +east+ 1)
(defconstant +west+ -1)
(defconstant +ne+ 17)
(defconstant +sw+ -17)
(defconstant +nw+ 15)
(defconstant +se+ -15)

(defconstant +opening-game+ 0)
(defconstant +middle-game+ 1)
(defconstant +end-game+ 2)
