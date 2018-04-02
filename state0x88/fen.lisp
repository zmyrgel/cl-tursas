;;;; fen0x88.lisp has utilities converting data from FEN strings to/from 0x88 board.

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
                (strcat x y))
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

(defun fen-board-parse (fen-string)
  "Utility function to help in converting given FEN-STRING to 0x88 board."
  (loop for rank in (reverse
                    (split-sequence
                     #\/
                     (loop for c across fen-string
                           if (digit-char-p c)
                             nconc (make-list (digit-char-p c) :initial-element #\E)
                           else
                             collect c)))
        nconc (nconc rank (make-list 8 :initial-element #\E))))

(defun fen-board->0x88board (fen-board)
  "Converts FEN-BOARD string to a 0x88 board representation."
  (loop with board = (init-game-board)
        for piece in (fen-board-parse fen-board)
        for index upfrom 0
        do (fill-square! board index (piece-value piece))
        finally (return board)))

(defun make-fen-row (board rank)
  "Builds single fen row from given board and row index."
  (strcat (compact-item #\E (loop for n below 8
                                  collect (piece-name (board-ref board (+ rank n)))))))

(defun board->fen-board (board)
  "Convert the given state's board to fen board field."
  (format nil "~{~A~^/~}" (loop for i from #x70 downto #x0 by #x10
                               collect (make-fen-row board i))))

(defun add-pieces (state)
  "Adds all pieces from board to piece-map."
  (do-board
      (let ((piece (board-ref (State0x88-board state) it)))
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
