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

(define-constant +castling-values+ '((8 . #\K)
                                     (4 . #\Q)
                                     (2 . #\k)
                                     (1 . #\q))
  :test 'equal)

(defmacro do-board (&rest body)
  "Utility to iterate board indexes.
   Binds current index to IT."
  `(dolist (it (list ,@(loop for x upto #x77
                             when (board-index-p x)
                               collect x)))
     ,@body))

(defun castling->str (castling)
  "Converts internal castling representation to string."
  (if (zerop castling)
      "-"
      (reduce (lambda (x y)
                (concatenate 'string x y))
              (mapcar (lambda (pair)
                        (when (plusp (logand castling (first pair)))
                          (string (rest pair))))
                      +castling-values+)
              :initial-value "")))

(defun castling->value (s)
  "Convers string representing castling to
   internal castling value."
  (reduce (lambda (result castling-pair)
            (if (some (lambda (c) (eql c (rest castling-pair))) s)
                (+ result (first castling-pair))
                result))
          +castling-values+ :initial-value 0))

(defun seek-king-index (state player)
  "Seeks king's index from piece-map.
   This is only used when generating state from a fen.
   Otherwise the king index can be queried from the board directly."
  (multiple-value-bind (king pieces)
      (if (= player +white+)
          (values +white-king+ (State0x88-white-pieces state))
          (values +black-king+ (State0x88-black-pieces state)))
    (loop for (index . piece) in pieces
          when (= piece king)
            return index)))

(defun fen-board->0x88board (s)
  "Converts string given in FEN notation to 0x88 board representation."
  (let ((board (init-game-board))
        (fen-list (map 'list #'identity s)))
    (dolist (pair (string-indexed (reduce (lambda (x y)
                                            (concatenate 'string x y))
                                          (mapcar (lambda (row) (concatenate 'string row "EEEEEEEE"))
                                                  (reverse (split-sequence #\/ (expand-digits #\E fen-list))))
                                          :initial-value "")))
      (fill-square! board (car pair) (piece-value (cdr pair))))
    board))

(defun make-fen-row (board row)
  "Builds single fen row from given board and row index."
  (apply #'str (compact-item #\E (mapcar (lambda (n)
                                           (piece-name (board-ref board (+ row n))))
                                         (loop for x below 8 collect x)))))

(defun board->fen-board (board)
  "Convert the given state's board to fen board field."
  (format nil "~{~a~^/~}" (mapcar (lambda (i)
                                    (make-fen-row board i))
                                  '(#x70 #x60 #x50 #x40 #x30 #x20 #x10 #x0))))

(defun add-pieces (state)
  "Adds all pieces from board to piece-map."
  (do-board (let ((piece (board-ref (State0x88-board state) it)))
              (cond ((black-piece-p piece)
                     (push (cons it piece) (State0x88-black-pieces state)))
                    ((white-piece-p piece)
                     (push (cons it piece) (State0x88-white-pieces state)))
                    (t nil)))))

(defun add-king-indexes (state)
  "Adds king indexes to state."
  (let ((black-king (seek-king-index state +black+))
        (white-king (seek-king-index state +white+))
        (board (State0x88-board state)))
    (update-king-index! board black-king +black+)
    (update-king-index! board white-king +white+)))

(defun add-full-moves (board moves)
  "Helper funtion to add full moves to board.
   Needed to workaround the byte limitation of the board."
  (let ((n-moves (truncate (/ moves 128))))
    (fill-square! board +full-move-n-store+ n-moves)
    (fill-square! board +full-move-store+ (- moves (* n-moves 128)))))

(defun parse-fen (s state)
  "Parses information from given FEN and applies it to given state."
  (destructuring-bind (board turn cast en-pass half full)
      (split-sequence #\space s)
    (let ((game-board (fen-board->0x88board board)))
      (fill-square! game-board +turn-store+ (if (string= turn "w") +white+ +black+))
      (fill-square! game-board +castling-store+ (castling->value cast))
      (fill-square! game-board +en-passant-store+ (if (string= en-pass "-")
                                                 -1
                                                 (coord->index en-pass)))
      (fill-square! game-board +half-move-store+ (parse-integer half))
      (add-full-moves game-board (parse-integer full))
      (setf (State0x88-board state) game-board)
      (add-pieces state)
      (add-king-indexes state)
      state)))

(defun parse-state (state)
  "Returns FEN representation of given game state."
  (let ((board (State0x88-board state)))
    (format nil "~a ~a ~a ~a ~a ~a"
            (board->fen-board board)
            (if (= (board-ref board +turn-store+) +white+) "w" "b")
            (castling->str (board-ref board +castling-store+))
            (let ((en-passant (board-ref board +en-passant-store+)))
              (if (= en-passant -1)
                  "-"
                  (index->coord en-passant)))
            (board-ref board +half-move-store+)
            (+ (* (board-ref board +full-move-n-store+) 127)
               (board-ref board +full-move-store+)))))
