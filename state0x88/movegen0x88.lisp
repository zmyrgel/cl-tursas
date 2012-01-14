;; Copyright (c) 2011, Timo Myyrä <timo.myyra@gmail.com>

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

(define-constant +rook-directions+ (list +north+ +south+ +east+ +west+)
  :test 'equal)
(define-constant +bishop-directions+ (list +nw+ +sw+ +ne+ +se+)
  :test 'equal)
(define-constant +queen-directions+ (concatenate 'list
                                                 +rook-directions+
                                                 +bishop-directions+)
  :test 'equal)

(define-constant +king-movement+ +queen-directions+
  :test 'equal)
(define-constant +black-pawn-movement+ (list +se+ +sw+ +south+)
  :test 'equal)
(define-constant +white-pawn-movement+ (list +ne+ +nw+ +north+)
  :test 'equal)
(define-constant +knight-movement+ (list -33 -31 -18 -14 14 18 31 33)
  :test 'equal)

(defun pmap-add! (state player index piece)
  "Add piece to player piece-map store on the board."
  (declare (bit player)
           (board-value index piece))
  (setf (gethash index (pmap-get state player)) piece))

(defun pmap-remove! (state player index)
  "Remove piece from player piece-map store on the board."
  (declare (bit player)
           (board-value index))
  (remhash index (pmap-get state player)))

(defun pmap-get (state player)
  "Returns the players piece-map from board."
  (declare (bit player))
  (if (= player +white+)
      (State0x88-white-pieces state)
      (State0x88-black-pieces state)))

(defun set-dynamic! (state value)
  "Sets the states dynamic value, now only set on captures."
  (declare (bit value))
  (fill-square! (State0x88-board state) +dynamic-store+ value))

(defun add-piece! (state index piece)
  "Associates given piece to states board."
  (declare (board-value index piece))
  (multiple-value-bind (player king)
      (if (white-piece-p piece)
          (values +white+ +white-king+)
          (values +black+ +black-king+))
    (let ((board (State0x88-board state)))
      (if (= piece king)
          (progn (update-king-index! board index player)
                 (fill-square! board index piece))
          (fill-square! (State0x88-board state) index piece)))
    (pmap-add! state player index piece)))

(defun remove-piece! (state index)
  "Removes piece from board and updates maps accordingly."
  (declare (board-value index))
  (let ((player (if (white-piece-p (board-ref (State0x88-board state) index))
                    +white+
                    +black+)))
    (progn (clear-square! (State0x88-board state) index)
           (pmap-remove! state player index))))

(defun move-piece! (state move)
  "Moves piece in the board."
  (let ((piece (the board-value (board-ref (State0x88-board state) (Move0x88-from move))))
        (occupant (the board-value (board-ref (State0x88-board state) (Move0x88-to move)))))
    (when (not (= occupant +empty-square+))
      (remove-piece! state (Move0x88-to move)))
    (remove-piece! state (Move0x88-from move))
    (add-piece! state (Move0x88-to move) piece)
    (set-dynamic! state (if (= occupant +empty-square+) 0 1))))

(defun move-castling-pieces! (player state move castling-side)
  "Helper function for update-board to make castling move on board.
   Mainly it moves the king piece and the participating rook piece."
  (declare (bit player)
           (board-value castling-side))
  (multiple-value-bind (rook king from to)
      (if (= player +white+)
          (values +white-rook+ +white-king+
                  (make-array 2 :initial-contents '(#x00 #x07))
                  (make-array 2 :initial-contents '(#x03 #x05)))
          (values +black-rook+ +black-king+
                  (make-array 2 :initial-contents '(#x70 #x77))
                  (make-array 2 :initial-contents '(#x73 #x75))))
    (progn
      (remove-piece! state (Move0x88-from move))
      (remove-piece! state (aref from castling-side))
      (add-piece! state (Move0x88-to move) king)
      (add-piece! state (aref to castling-side) rook))))

(defun piece-indexes (state player)
  "Gets a list of all board indexes containing
   player's pieces in given board."
  (declare (bit player))
  (hash-table-keys (pmap-get state player)))

(defun castlingp (piece move)
  "Checks given move is castling move."
  (declare (board-value piece))
  (and (or (= piece +white-king+)
           (= piece +black-king+))
       (= 2 (the board-value (abs (- (the board-value (Move0x88-to move))
                                     (the board-value (Move0x88-from move))))))))

(defun en-passant-p (board piece move)
  "Is the given move an en-passant move"
  (declare (board-vector board)
           (board-value piece))
  (let ((movement (the board-value (- (Move0x88-to move) (Move0x88-from move)))))
    (and (or (= piece +white-pawn+)
             (= piece +black-pawn+))
         (some (lambda (dir)
                 (declare (board-value dir))
                 (= dir movement)) (list +sw+ +se+ +ne+ +nw+))
         (empty-square-p board (the board-value (Move0x88-to move))))))

(defun slide-in-dir (player board index dir)
  "Returns a list of possible moves by sliding piece
   from index to given direction on the board.
   Sliding will continue until it hits piece or board edge."
  (declare (board-vector board)
           (bit player)
           (board-value index dir))
  (labels ((slider (new-index moves)
             (if (or (not (board-index-p new-index))
                     (occupied-by-p board new-index player))
                 moves
                 (if (empty-square-p board new-index)
                     (slider (+ new-index dir)
                             (cons (make-move index new-index 0)
                                   moves))
                     (cons (make-move index new-index 0)
                           moves)))))
    (slider (+ index dir) nil)))

(defun move-to-place (player board index place)
  "Return list of moves for given piece."
  (declare (bit player)
           (board-vector board)
           (board-value index place))
  (let ((new-place (the board-value (+ index place))))
    (when (and (board-index-p new-place)
               (or (empty-square-p board new-place)
                   (occupied-by-p board new-place (opponent player))))
      (list (make-move index new-place 0)))))

(defun ray-to-pieces-p (board index dir pieces)
  "Checks if there's ray from index to given pieces."
  (declare (board-vector board)
           (board-value index dir))
  (let ((new-index (the fixnum (+ index dir))))
    (cond ((not (board-index-p new-index)) nil)
          ((empty-square-p board new-index) (ray-to-pieces-p board new-index dir pieces))
          (t (some (lambda (piece)
                     (declare (board-value piece))
                     (= (the board-value (board-ref board new-index)) piece))
                   pieces)))))

(defun threaten-by-piece-p (board index piece places)
  "Can piece in index be captured by opponents pieces."
  (declare (board-vector board)
           (board-value index piece))
  (some (lambda (place)
          (let ((new-index (the fixnum (+ index place))))
            (when (and (board-index-p new-index)
                       (= (the board-value (board-ref board new-index)) piece))
              t)))
        places))

(defun threaten-by-slider-p (board index pieces directions)
  "Can the piece in index of board be captured
   by opponents queen or rook?"
  (declare (board-vector board)
           (board-value index))
  (some (lambda (dir)
          (declare (board-value dir))
          (ray-to-pieces-p board index dir pieces))
        directions))

(defun threaten-by-king-p (board index opp)
  "Can the piece in index on board be captured by opponents king.
   Checks this by looking for a king within next squares and then
   checking if it can move to index and not be threatened instead."
  (declare (board-vector board)
           (board-value index)
           (bit opp))
  (multiple-value-bind (player king opp-king)
      (if (= opp +white+)
          (values +black+ +black-king+ +white-king+)
          (values +white+ +white-king+ +black-king+))
    (let ((opp-king-idx (the fixnum (king-index board opp))))
      (when (some (lambda (offset)
                    (declare (board-value offset))
                    (when (= opp-king-idx (the fixnum (+ index offset)))
                      t))
                  +king-movement+)
        (if (= (the board-value (board-ref board index)) king)
            t
            (let ((temp-board (copy-array board)))
              (clear-square! temp-board opp-king-idx)
              (fill-square! temp-board index opp-king)
              (not (threatenedp temp-board index player))))))))

(defun threaten-by-white-p (board index)
  "Checks if given index is threatened by white player."
  (declare (board-vector board)
           (board-value index))
  (let ((by-piece-p (curry #'threaten-by-piece-p board index))
        (by-slider-p (curry #'threaten-by-slider-p board index)))
    (or (funcall by-piece-p +white-knight+ +knight-movement+)
        (funcall by-slider-p (list +white-queen+ +white-rook+) +rook-directions+)
        (funcall by-slider-p (list +white-queen+ +white-bishop+) +bishop-directions+)
        (funcall by-piece-p +white-pawn+ (list +se+ +sw+))
        (threaten-by-king-p board index +white+))))

(defun threaten-by-black-p (board index)
  "Checks if given index is threatened by black player."
  (declare (board-vector board)
           (board-value index))
  (let ((by-piece-p (curry #'threaten-by-piece-p board index))
        (by-slider-p (curry #'threaten-by-slider-p board index)))
    (or (funcall by-piece-p +black-knight+ +knight-movement+)
        (funcall by-slider-p (list +black-queen+ +black-rook+) +rook-directions+)
        (funcall by-slider-p (list +black-queen+ +black-bishop+) +bishop-directions+)
        (funcall by-piece-p +black-pawn+ (list +ne+ +nw+))
        (threaten-by-king-p board index +black+))))

(defun threatenedp (board index opponent)
  "Checks if given index on board is threatened by opponent."
  (declare (board-vector board)
           (board-value index)
           (bit opponent))
  (if (= opponent +white+)
      (threaten-by-white-p board index)
      (threaten-by-black-p board index)))

(defun legal-castling-p (player board index dir)
  "Predicate to check if castling is possible on the board."
  (declare (bit player)
           (board-vector board)
           (board-value index dir))
  (let* ((king-sq-1 (the fixnum (+ index dir)))
         (king-sq-2 (the fixnum (+ king-sq-1 dir)))
         (opponent (the bit (opponent player)))
         (safe-index-p (lambda (x)
                         (declare (fixnum x))
                         (and (empty-square-p board x)
                              (not (threatenedp board x opponent))))))
    (and (not (threatenedp board index opponent))
         (funcall safe-index-p king-sq-1)
         (funcall safe-index-p king-sq-2)
         (if (= dir +west+)
             (empty-square-p board (the fixnum (+ king-sq-2 dir)))
             t))))

(defun list-king-moves (player board index)
  "Returns a list of available moves for players king
   in given index on the board."
  (declare (bit player)
           (board-vector board)
           (board-value index))
  (let* ((castling (the board-value (board-ref board +castling-store+)))
         (castle-side-p (lambda (side dir)
                          (declare (board-value side dir))
                          (let ((value (if (= player +white+)
                                           (if (= side +king-side+) 8 4)
                                           (if (= side +king-side+) 2 1))))
                            (plusp (the board-value (logand value dir))))))
         (castling-move (lambda (side dir)
                          (declare (board-value side dir))
                          (when (and (funcall castle-side-p side castling)
                                     (legal-castling-p player board index dir))
                            (list (make-move index (the board-value (+ index dir dir)) 0))))))
    (concatenate 'list
                 (mapcan (curry #'move-to-place player board index) +king-movement+)
                 (funcall castling-move +king-side+ +east+)
                 (funcall castling-move +queen-side+ +west+))))

(defun make-pawn-move (player from to)
  "Utility function to create pawn moves.
   Needed to handle promotions."
  (declare (bit player)
           (board-value from to))
  (make-move from to
             (cond ((and (= player +white+)
                         (= (the board-value (row to)) #x70)) +white-queen+)
                   ((and (= player +black+)
                         (= (the board-value (row to)) #x00)) +black-queen+)
                   (t 0))))

(defun list-pawn-normal-moves (player board index)
  "Returns a list of normal pawn moves available
   for player in board index."
  (declare (bit player)
           (board-vector board)
           (board-value index))
  (let* ((dir (if (= player +white+) +north+ +south+))
         (move-index (the fixnum (+ index dir))))
    (when (and (board-index-p move-index)
               (empty-square-p board move-index))
      (if (and (board-index-p (the fixnum (+ move-index dir)))
               (empty-square-p board (the board-value (+ move-index dir)))
               (or (and (= player +white+)
                        (same-row-p index #x10))
                   (and (= player +black+)
                        (same-row-p index #x60))))
          (list (make-pawn-move player index move-index)
                (make-pawn-move player index (the board-value (+ move-index dir))))
          (list (make-pawn-move player index move-index))))))

(defun pawn-capture (player board index place)
  "Utility function to generate pawn capture moves.
   If pawn of index can capture piece in place, generate the move
   otherwise return nil."
  (declare (bit player)
           (board-vector board)
           (board-value index place))
  (when (or (and (board-index-p place)
                 (= (the board-value (board-ref board +en-passant-store+)) place))
            (and (board-index-p place)
                 (board-occupied-p board place)
                 (not (occupied-by-p board place player))))
    (list (make-pawn-move player index place))))

(defun list-pawn-moves (player board index)
  "Returns a list of available pawn moves
   for player's pawn in board index."
  (declare (bit player)
           (board-vector board)
           (board-value index))
  (concatenate 'list
               (list-pawn-normal-moves player board index)
               (mapcan (curry #'pawn-capture player board index)
                       (if (= player +white+)
                           (list (the fixnum (+ index +nw+)) (the fixnum (+ index +ne+)))
                           (list (the fixnum (+ index +sw+)) (the fixnum (+ index +se+)))))))

(defun piece-moves (board player index piece)
  "Returns a list of possible piece moves in board index."
  (declare (board-vector board)
           (bit player)
           (board-value index piece))
  (let ((slider (lambda (directions)
                  (mapcan (curry #'slide-in-dir player board index) directions)))
        (mover (lambda (movement)
                 (mapcan (curry #'move-to-place player board index) movement))))
    (cond ((or (= piece +white-pawn+)
               (= piece +black-pawn+)) (list-pawn-moves player board index))
          ((or (= piece +white-bishop+)
               (= piece +black-bishop+)) (funcall slider +bishop-directions+))
          ((or (= piece +white-knight+)
               (= piece +black-knight+)) (funcall mover +knight-movement+))
          ((or (= piece +white-rook+)
               (= piece +black-rook+)) (funcall slider +rook-directions+))
          ((or (= piece +white-queen+)
               (= piece +black-queen+)) (funcall slider +queen-directions+))
          ((or (= piece +white-king+)
               (= piece +black-king+)) (list-king-moves player board index)))))

(defun pseudo-moves (player state)
  "Lists all pseudo-moves for player in state.
   Note: moves generated can leave player in check, hence pseudo-moves."
  (declare (bit player))
  (loop for index being the hash-keys in (pmap-get state player)
        using (hash-value piece)
        nconc (piece-moves (State0x88-board state) player index piece)))

(defun allowed-move-p (state move)
  "Checks if given move is allowed in state.
   Prevents players from moving each others pieces."
  (let ((player (the board-value (board-ref (State0x88-board state) +turn-store+)))
        (piece (the board-value (board-ref (State0x88-board state) (Move0x88-from move)))))
    (and (occupied-by-p (State0x88-board state) (Move0x88-from move) player)
         (some (lambda (m)
                 (and (= (the board-value (Move0x88-from move)) (the board-value (Move0x88-from m)))
                      (= (the board-value (Move0x88-to move)) (the board-value (Move0x88-to m)))))
               (piece-moves (State0x88-board state) player (the board-value (Move0x88-from move)) piece)))))
