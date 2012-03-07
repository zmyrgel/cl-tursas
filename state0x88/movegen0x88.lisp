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
  (setf (gethash index (pmap-get state player)) piece))

(defun pmap-remove! (state player index)
  "Remove piece from player piece-map store on the board."
  (remhash index (pmap-get state player)))

(defun pmap-get (state player)
  "Returns the players piece-map from board."
  (if (= player +white+)
      (State0x88-white-pieces state)
      (State0x88-black-pieces state)))

(defun set-dynamic! (state value)
  "Sets the states dynamic value, now only set on captures."
  (fill-square! (State0x88-board state) +dynamic-store+ value))

(defun add-piece! (state index piece)
  "Associates given piece to states board."
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
  (let ((player (if (white-piece-p (board-ref (State0x88-board state) index))
                    +white+
                    +black+)))
    (progn (clear-square! (State0x88-board state) index)
           (pmap-remove! state player index))))

(defun move-piece! (state move)
  "Moves piece in the board."
  (let ((piece (board-ref (State0x88-board state) (Move0x88-from move)))
        (occupant (board-ref (State0x88-board state) (Move0x88-to move))))
    (when (not (= occupant +empty-square+))
      (remove-piece! state (Move0x88-to move)))
    (remove-piece! state (Move0x88-from move))
    (add-piece! state (Move0x88-to move) piece)
    (set-dynamic! state (if (= occupant +empty-square+) 0 1))))

(defun move-castling-pieces! (player state move castling-side)
  "Helper function for update-board to make castling move on board.
   Mainly it moves the king piece and the participating rook piece."
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
  (hash-table-keys (pmap-get state player)))

(defun castlingp (piece move)
  "Checks given move is castling move."
  (and (or (= piece +white-king+)
           (= piece +black-king+))
       (= 2 (abs (- (Move0x88-to move)
                    (Move0x88-from move))))))

(defun en-passant-p (board piece move)
  "Is the given move an en-passant move"
  (let ((movement (- (Move0x88-to move) (Move0x88-from move))))
    (and (or (= piece +white-pawn+)
             (= piece +black-pawn+))
         (some (lambda (dir)
                 (= dir movement)) (list +sw+ +se+ +ne+ +nw+))
         (empty-square-p board (Move0x88-to move)))))

(defun slide-in-dir (player board index dir)
  "Returns a list of possible moves by sliding piece
   from index to given direction on the board.
   Sliding will continue until it hits piece or board edge."
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
  (let ((new-place (+ index place)))
    (when (and (board-index-p new-place)
               (or (empty-square-p board new-place)
                   (occupied-by-p board new-place (opponent player))))
      (list (make-move index new-place 0)))))

(defun ray-to-pieces-p (board index dir pieces)
  "Checks if there's ray from index to given pieces."
  (let ((new-index (+ index dir)))
    (cond ((not (board-index-p new-index)) nil)
          ((empty-square-p board new-index) (ray-to-pieces-p board new-index dir pieces))
          (t (some (lambda (piece)
                     (= (board-ref board new-index) piece))
                   pieces)))))

(defun threaten-by-piece-p (board index piece places)
  "Can piece in index be captured by opponents pieces."
  (some (lambda (place)
          (let ((new-index (+ index place)))
            (when (and (board-index-p new-index)
                       (= (board-ref board new-index) piece))
              t)))
        places))

(defun threaten-by-slider-p (board index pieces directions)
  "Can the piece in index of board be captured
   by opponents queen or rook?"
  (some (lambda (dir)
          (ray-to-pieces-p board index dir pieces))
        directions))

(defun threaten-by-king-p (board index opp)
  "Can the piece in index on board be captured by opponents king.
   Checks this by looking for a king within next squares and then
   checking if it can move to index and not be threatened instead."
  (multiple-value-bind (player king opp-king)
      (if (= opp +white+)
          (values +black+ +black-king+ +white-king+)
          (values +white+ +white-king+ +black-king+))
    (let ((opp-king-idx (king-index board opp)))
      (when (some (lambda (offset)
                    (when (= opp-king-idx (+ index offset))
                      t))
                  +king-movement+)
        (if (= (board-ref board index) king)
            t
            (unwind-protect
                 (progn (clear-square! board opp-king-idx)
                        (fill-square! board index opp-king)
                        (not (threatenedp board index player)))
              (clear-square! board index)
              (fill-square! board opp-king-idx opp-king)))))))

(defun threaten-by-white-p (board index)
  "Checks if given index is threatened by white player."
  (flet ((by-piece-p (piece places)
           (threaten-by-piece-p board index piece places))
         (by-slider-p (pieces directions)
           (threaten-by-slider-p board index pieces directions)))
    (or (by-piece-p +white-knight+ +knight-movement+)
        (by-slider-p (list +white-queen+ +white-rook+) +rook-directions+)
        (by-slider-p (list +white-queen+ +white-bishop+) +bishop-directions+)
        (by-piece-p +white-pawn+ (list +se+ +sw+))
        (threaten-by-king-p board index +white+))))

(defun threaten-by-black-p (board index)
  "Checks if given index is threatened by black player."
  (flet ((by-piece-p (piece places)
           (threaten-by-piece-p board index piece places))
         (by-slider-p (pieces directions)
           (threaten-by-slider-p board index pieces directions)))
    (or (by-piece-p +black-knight+ +knight-movement+)
        (by-slider-p (list +black-queen+ +black-rook+) +rook-directions+)
        (by-slider-p (list +black-queen+ +black-bishop+) +bishop-directions+)
        (by-piece-p +black-pawn+ (list +ne+ +nw+))
        (threaten-by-king-p board index +black+))))

(defun threatenedp (board index opponent)
  "Checks if given index on board is threatened by opponent."
  (if (= opponent +white+)
      (threaten-by-white-p board index)
      (threaten-by-black-p board index)))

(defun legal-castling-p (player board index dir)
  "Predicate to check if castling is possible on the board."
  (flet ((safe-index-p (index)
           (and (empty-square-p board index)
                (not (threatenedp board index (opponent player))))))
    (and (not (threatenedp board index (opponent player)))
         (safe-index-p (+ index dir))
         (safe-index-p (+ index dir dir))
         (if (= dir +west+)
             (empty-square-p board (+ index dir dir dir))
             t))))

(defun list-king-moves (player board index)
  "Returns a list of available moves for players king
   in given index on the board."
  (labels ((castle-side-p (side dir)
             (multiple-value-bind (king-side queen-side)
                 (if (= player +white+)
                     (values 8 4)
                     (values 2 1))
               (plusp (logand (if (= side +king-side+)
                                  king-side
                                  queen-side)
                              dir))))
           (castling-move (side dir)
             (when (and (castle-side-p side (board-ref board +castling-store+))
                        (legal-castling-p player board index dir))
               (list (make-move index (+ index dir dir) 0)))))
    (concatenate 'list
                 (mapcan (lambda (m)
                           (move-to-place player board index m))
                         +king-movement+)
                 (castling-move +king-side+ +east+)
                 (castling-move +queen-side+ +west+))))

(defun make-pawn-move (player from to)
  "Utility function to create pawn moves.
   Needed to handle promotions."
  (make-move from to
             (cond ((and (= player +white+)
                         (= (row to) #x70)) +white-queen+)
                   ((and (= player +black+)
                         (= (row to) #x00)) +black-queen+)
                   (t 0))))

(defun list-pawn-normal-moves (player board index)
  "Returns a list of normal pawn moves available
   for player in board index."
  (let* ((dir (if (= player +white+) +north+ +south+))
         (move-index (+ index dir)))
    (when (and (board-index-p move-index)
               (empty-square-p board move-index))
      (if (and (board-index-p (+ move-index dir))
               (empty-square-p board (+ move-index dir))
               (or (and (= player +white+)
                        (same-row-p index #x10))
                   (and (= player +black+)
                        (same-row-p index #x60))))
          (list (make-pawn-move player index move-index)
                (make-pawn-move player index (+ move-index dir)))
          (list (make-pawn-move player index move-index))))))

(defun pawn-capture (player board index place)
  "Utility function to generate pawn capture moves.
   If pawn of index can capture piece in place, generate the move
   otherwise return nil."
  (when (or (and (board-index-p place)
                 (= (board-ref board +en-passant-store+) place))
            (and (board-index-p place)
                 (board-occupied-p board place)
                 (not (occupied-by-p board place player))))
    (list (make-pawn-move player index place))))

(defun list-pawn-moves (player board index)
  "Returns a list of available pawn moves
   for player's pawn in board index."
  (concatenate 'list
               (list-pawn-normal-moves player board index)
               (mapcan (lambda (p)
                         (pawn-capture player board index p))
                       (if (= player +white+)
                           (list (+ index +nw+) (+ index +ne+))
                           (list (+ index +sw+) (+ index +se+))))))

;; XXX: dispatch table based on piece type
(defun piece-moves (board player index piece)
  "Returns a list of possible piece moves in board index."
  (flet ((slider (directions)
           (mapcan (lambda (d)
                     (slide-in-dir player board index d))
                   directions))
         (mover (movement)
           (mapcan (lambda (m)
                     (move-to-place player board index m))
                   movement)))
    (cond ((or (= piece +white-pawn+)
               (= piece +black-pawn+)) (list-pawn-moves player board index))
          ((or (= piece +white-bishop+)
               (= piece +black-bishop+)) (slider +bishop-directions+))
          ((or (= piece +white-knight+)
               (= piece +black-knight+)) (mover +knight-movement+))
          ((or (= piece +white-rook+)
               (= piece +black-rook+)) (slider +rook-directions+))
          ((or (= piece +white-queen+)
               (= piece +black-queen+)) (slider +queen-directions+))
          ((or (= piece +white-king+)
               (= piece +black-king+)) (list-king-moves player board index)))))

(defun pseudo-moves (player state)
  "Lists all pseudo-moves for player in state.
   Note: moves generated can leave player in check, hence pseudo-moves."
  (loop for index being the hash-keys in (pmap-get state player)
        using (hash-value piece)
        nconc (piece-moves (State0x88-board state) player index piece)))

(defun allowed-move-p (state move)
  "Checks if given move is allowed in state.
   Prevents players from moving each others pieces."
  (let ((player (board-ref (State0x88-board state) +turn-store+))
        (piece (board-ref (State0x88-board state) (Move0x88-from move))))
    (and (occupied-by-p (State0x88-board state) (Move0x88-from move) player)
         (some (lambda (m)
                 (and (= (Move0x88-from move) (Move0x88-from m))
                      (= (Move0x88-to move) (Move0x88-to m))))
               (piece-moves (State0x88-board state) player (Move0x88-from move) piece)))))
