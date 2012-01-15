;; Copyright (c) 2012, Timo Myyrä <timo.myyra@gmail.com>

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

(in-package :tursas.state0x88)

(declaim (inline white-piece-p
                 black-piece-p
                 occupied-by-p
                 opponent
                 promotionp
                 board-index-p
                 column
                 row
                 same-column-p
                 same-row-p
                 square-color
                 same-color-p
                 board-occupied-p
                 board-ref
                 fill-square!
                 clear-square!
                 king-index
                 update-king-index!))

(define-constant +board-color+
    (make-table (list +black+ +white+ +black+ +white+ +black+ +white+ +black+ +white+
                      +white+ +black+ +white+ +black+ +white+ +black+ +white+ +black+
                      +black+ +white+ +black+ +white+ +black+ +white+ +black+ +white+
                      +white+ +black+ +white+ +black+ +white+ +black+ +white+ +black+
                      +black+ +white+ +black+ +white+ +black+ +white+ +black+ +white+
                      +white+ +black+ +white+ +black+ +white+ +black+ +white+ +black+
                      +black+ +white+ +black+ +white+ +black+ +white+ +black+ +white+
                      +white+ +black+ +white+ +black+ +white+ +black+ +white+ +black+))
  :test 'equalp)

(defun white-piece-p (piece)
  (declare (board-value piece))
  "Predicate to check if given piece value belongs to white."
  (> piece +empty-square+))

(defun black-piece-p (piece)
  (declare (board-value piece))
  "Checks if given piece value belongs to black."
  (< piece +empty-square+))

(defun occupied-by-p (board index player)
  (declare (board-vector board)
           (board-value index)
           (bit player))
  "Checks if given board index is occupied by player."
  (let ((piece-color-p (if (= player +white+)
                           #'white-piece-p
                           #'black-piece-p)))
    (and (board-occupied-p board index)
         (funcall piece-color-p (board-ref board index)))))

(defun opponent (player)
  (declare (bit player))
  "Return opponent of given player"
  (the bit (logxor player 1)))

(defun promotionp (piece move)
  (declare (board-value piece))
  "Checks if given move is pawn promotion."
  (or (and (= piece +white-pawn+)
           (= (the board-value (row (Move0x88-to move))) 7))
      (and (= piece +black-pawn+)
           (= (the board-value (row (Move0x88-to move))) 0))))

(defun piece-name (piece)
  "Gives piece character representation from its board value."
  (declare (board-value piece))
  (cond ((= piece +white-king+) #\K)
        ((= piece +white-queen+) #\Q)
        ((= piece +white-bishop+) #\B)
        ((= piece +white-knight+) #\N)
        ((= piece +white-rook+) #\R)
        ((= piece +white-pawn+) #\P)
        ((= piece +black-king+) #\k)
        ((= piece +black-queen+) #\q)
        ((= piece +black-bishop+) #\b)
        ((= piece +black-knight+) #\n)
        ((= piece +black-rook+) #\r)
        ((= piece +black-pawn+) #\p)
        (t #\E)))

(defun piece-value (char)
  "Gives pieces character numerical representation from its char."
  (the board-value
       (case char
         (#\P +white-pawn+)
         (#\p +black-pawn+)
         (#\R +white-rook+)
         (#\r +black-rook+)
         (#\N +white-knight+)
         (#\n +black-knight+)
         (#\B +white-bishop+)
         (#\b +black-bishop+)
         (#\Q +white-queen+)
         (#\q +black-queen+)
         (#\K +white-king+)
         (#\k +black-king+)
         (otherwise +empty-square+))))

(defun board-index-p (index)
  "Does the given index represent a square on the board?"
  (declare (fixnum index))
  (zerop (the fixnum (logand index #x88))))

(defun empty-square-p (board index)
  "Checks if given index on board is empty."
  (declare (board-vector board)
           (board-value index))
  (zerop (the board-value (board-ref board index))))

(defun column (index)
  "Get the board column of the given square index.
   Columns start at 0 and go up to 7."
  (declare (board-value index))
  (the board-value (logand index 7)))

(defun row (index)
  "Get the board row of the given square index.
   Rows start at 0 and they go up to 7."
  (declare (board-value index))
  (the board-value (ash index -4)))

(defun same-column-p (x y)
  "Determines if both given square indexes x and x are on the same column."
  (declare (board-value x y))
  (= (column x) (column y)))

(defun same-row-p (x y)
  "Determines if both given square indexes x and y are on the same row."
  (declare (board-value x y))
  (= (row x) (row y)))

(defun square-color (sq)
  "Returns the color of given square."
  (declare (board-value sq))
  (the bit (board-ref +board-color+ sq)))

(defun same-color-p (sq1 sq2)
  "Check if two squares are same color."
  (declare (board-value sq1 sq2))
  (= (square-color sq1)
     (square-color sq2)))

(defun board-occupied-p (board index)
  "Predicate to check if board index is occupied or not."
  (declare (board-vector board)
           (board-value index))
  (not (empty-square-p board index)))

(defun init-game-board ()
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  (make-array 128 :initial-element 0 :element-type 'board-value))

(defun board-ref (board index)
  "Getter for board"
  (declare (board-vector board)
           (board-value index))
  (the board-value (aref board index)))

(defun fill-square! (board index value)
  "Return new board with given value added to given board's index."
  (declare (board-vector board)
           (board-value index value))
  (setf (aref board index) value)
  nil)

(defun clear-square! (board index)
  "Clears the given square index on the game board."
  (declare (board-vector board)
           (board-value index))
  (fill-square! board index +empty-square+))

(defun king-index (board player)
  "Returns the players king's index on the board."
  (declare (board-vector board)
           (bit player))
  (the board-value (board-ref board (if (= player +white+)
                                        +white-king-store+
                                        +black-king-store+))))

(defun update-king-index! (board index player)
  "Updates index of given player's king in outer board."
  (declare (board-vector board)
           (board-value index)
           (bit player))
  (fill-square! board (if (= player +white+)
                          +white-king-store+
                          +black-king-store+)
                index))
