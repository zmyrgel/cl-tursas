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

(defun calculate-castling (player castling move)
  "Utility to calculate new castling value after white players move.
   Castling value is updated if either our king or rook moves
   or opponents rook gets captured.
   Castling value is kept as a number and is operated at bit level.
   Castling value is composesd as: K = 8, Q = 4, k = 2, q = 1"
  (multiple-value-bind (king-m l-rook-m r-rook-m l-rook-c r-rook-c)
      (if (= player +white+)
          (values 3 11 7 14 13)
          (values 12 13 13 11 7))
    (cond ((= (Move0x88-from move) #x04) (logand castling king-m))
          ((= (Move0x88-from move) #x00) (logand castling l-rook-m))
          ((= (Move0x88-from move) #x07) (logand castling r-rook-m))
          ((= (Move0x88-to move) #x70) (logand castling l-rook-c))
          ((= (Move0x88-to move) #x77) (logand castling r-rook-c))
          (t castling))))

(defun inc-full-moves! (board)
  "Utility to increase full moves on the board.
   Uses two vector indexes because of the limitation of byte value.
   If full moves get to 127 increase multiplier store and reduce full move
   store to 0. This gets full move count to get high enough."
  (let ((moves (board-ref board +full-move-store+))
        (n-moves (board-ref board +full-move-n-store+)))
    (if (= moves 127)
        (progn (fill-square! board +full-move-n-store+ (1+ n-moves))
               (fill-square! board +full-move-store+ 0))
        (fill-square! board +full-move-store+ (1+ moves)))))

(defun promotion-piece (player move)
  "Helper function to return promotion piece value.
    Reads the promotion piece value from move, defaults to queen."
  (let ((piece (Move0x88-promotion move)))
    (if (zerop piece)
        (if (= player +white+)
            +white-queen+
            +black-queen+)
        (if (= player +white+)
            (- piece)
            piece))))

(defun fifty-move-rule-p (state)
  "Checks if state is draw according to 50-move rule."
  (>= (board-ref (State0x88-board state) +half-move-store+) 50))

(defun stalematep (state)
  "Check if given state is in stalemate."
  (and (not (checkp state))
       (null (legal-states state))))

(defun fide-draw-p (state)
  "Checks if state is draw according to FIDE rules:
   - Both sides have only king piece.
   - One side has king and bishop or knight vs. others king
   - One sides king and two knights agains others bare king
   - Both sides have only bishop of same color besides kings"
  (let* ((piece-list (append (State0x88-white-pieces state)
                             (State0x88-black-pieces state)))
         (indexes (mapcar #'car piece-list))
         (pieces (mapcar #'cdr piece-list))
         (piece-count (list-length indexes)))
    (and (<= piece-count 4)
         (or (= piece-count 2)
             (and (= piece-count 3)
                  (some (lambda (p)
                          (or (= +black-knight+ p)
                              (= +black-bishop+ p)
                              (= +white-knight+ p)
                              (= +white-bishop+ p)))
                        pieces))
             (and (= piece-count 4)
                  (or (= 2 (list-length (remove-if (lambda (p)
                                                     (= +black-knight+ p))
                                                   pieces)))
                      (= 2 (list-length (remove-if (lambda (p)
                                                     (= +white-knight+ p))
                                                   pieces)))
                      (let ((bishops (remove-if (lambda (i)
                                                  (or (= +black-bishop+ (cdr (assoc i piece-list)))
                                                      (= +white-bishop+ (cdr (assoc i piece-list)))))
                                                indexes)))
                        (when (not (< (list-length bishops) 2))
                          (same-color-p (first (mapcar #'car bishops))
                                        (second (mapcar #'cdr bishops)))))))))))

(defun repetitionp (state)
  "Predicate to see if game is draw by repetition.
   Stub function to be filled later."
  (declare (ignore state))
  nil)

(defun update-move (state move)
  "Update the previous move of board.
   Stores previous move to 'off-board' locations"
  (let ((board (State0x88-board state)))
    (fill-square! board +prev-move-from+ (Move0x88-from move))
    (fill-square! board +prev-move-to+ (Move0x88-to move))
    (fill-square! board +prev-piece+ (board-ref board (Move0x88-from move)))
    state))

(defun pawn-or-capture-move-p (board move)
  "Predicate to see if move was pawn move or a capture"
  (let ((moving-piece (board-ref board (Move0x88-from move))))
    (or (= moving-piece +white-pawn+)
        (= moving-piece +black-pawn+)
        (not (= (board-ref board (Move0x88-to move)) +empty-square+)))))

(defun update-half-moves (state move)
  "Increases half move count on board unless the move
   was pawn or a capture move."
  (let ((board (State0x88-board state)))
    (fill-square! board +half-move-store+
                  (if (pawn-or-capture-move-p board move)
                      0
                      (1+ (board-ref board +half-move-store+))))
    state))

(defun update-board (state move)
  "Returns state with new board after applying move to state."
  (let* ((board (State0x88-board state))
         (player (board-ref board +turn-store+))
         (moving-piece (board-ref board (Move0x88-from move))))
    (cond ((promotionp moving-piece move)
           (remove-piece! state (Move0x88-from move))
           (add-piece! state (Move0x88-to move) (promotion-piece player move)))
          ((castlingp moving-piece move)
           (move-castling-pieces! player state move))
          ((en-passant-p board moving-piece move)
           (move-piece! state move)
           (remove-piece! state (+ (Move0x88-to move)
                                   (if (= player +white+)
                                       +south+
                                       +north+))))
          (t (move-piece! state move)))
    state))

(defun update-player-check (state)
  "Checks that players move won't leave the players king in check."
  (let* ((board (State0x88-board state))
         (player (board-ref board +turn-store+)))
    (when (not (threatenedp board (king-index board player) (opponent player)))
      state)))

(defun update-castling (state move)
  "Updates states castling value by checking move with current castling value."
  (let* ((board (State0x88-board state))
         (castling (board-ref board +castling-store+))
         (player (board-ref board +turn-store+)))
    (if (zerop castling)
        state
        (progn (fill-square! board +castling-store+ (calculate-castling player castling move))
               state))))

(defun update-en-passant (state move)
  "Associates new en-passant value with given state based on the move."
  (let ((board (State0x88-board state)))
    (fill-square! board +en-passant-store+
                  (let ((piece (board-ref board (Move0x88-to move)))
                        (opp-pawn (if (= (board-ref board +turn-store+) +white+)
                                      +black-pawn+ +white-pawn+))
                        (from (Move0x88-from move))
                        (to (Move0x88-to move)))
                    (if (and (or (= piece +white-pawn+)
                                 (= piece +black-pawn+))
                             (= (abs (- to from)) #x20)
                             (let ((west-index (+ to +west+))
                                   (east-index (+ to +east+)))
                               (or (and (board-index-p west-index)
                                        (= opp-pawn (board-ref board west-index)))
                                   (and (board-index-p east-index)
                                        (= opp-pawn (board-ref board east-index))))))
                        (/ (+ to from) 2)
                        -1)))
    state))

(defun update-full-moves (state)
  "Updates full move count on board."
  (let ((board (State0x88-board state)))
    (when (= (board-ref board +turn-store+) +black+)
      (inc-full-moves! board))
    state))

(defun update-opponent-check (state)
  "Updates opponents check status bit on the state.
   Enables check bit in state if opponents king is threatened."
  (let* ((board (State0x88-board state))
         (player (board-ref board +turn-store+)))
    (fill-square! board +check-store+
                  (if (threatenedp board
                                   (king-index board (opponent player))
                                   player)
                      1
                      0))
    state))

(defun update-turn (state)
  "Updates player turn value on board."
  (let ((board (State0x88-board state)))
    (fill-square! board +turn-store+ (opponent (board-ref board +turn-store+)))
    state))

(defun update-state (state move)
  "Updates game state to reflect changes from move.
   If game state is not legal, will return a nil value."
  (let ((new-state (copy-state state)))
    (update-move new-state move)
    (update-half-moves new-state move)
    (update-board new-state move)
    (when (update-player-check new-state)
      (update-castling new-state move)
      (update-en-passant new-state move)
      (update-full-moves new-state)
      (update-opponent-check new-state)
      (update-turn new-state))))

(defun check-situation (state)
  "Checks which situation, opening, middle or end-game the game is."
  (let ((pieces (append (State0x88-white-pieces state)
                        (State0x88-black-pieces state))))
    (cond ((< (list-length (mapcar #'car pieces)) 15) +end-game+)
          ((> (board-ref (State0x88-board state) +full-move-store+) 10) +middle-game+)
          (t +opening-game+))))

(defstruct State0x88
  (board (init-game-board) :type board-vector)
  (black-pieces nil :type list)
  (white-pieces nil :type list))

(defmethod allowedp ((state State0x88) (move Move0x88))
  (allowed-move-p state move))

(defmethod occupiedp ((state State0x88) (index integer))
  (board-occupied-p (State0x88-board state) index))

(defmethod blackp ((state State0x88) (index integer))
  (occupied-by-p (State0x88-board state) index +black+))

(defmethod whitep ((state State0x88) (index integer))
  (occupied-by-p (State0x88-board state) index +white+))

(defmethod checkp ((state State0x88))
  (= (the bit (board-ref (State0x88-board state) +check-store+)) 1))

(defmethod matep ((state State0x88))
  (and (checkp state)
       (null (legal-states state))))

(defmethod drawp ((state State0x88))
  (or (fifty-move-rule-p state)
      (fide-draw-p state)
      (stalematep state)
      (repetitionp state)))

(defmethod result ((state State0x88))
  (cond ((fifty-move-rule-p state) "1/2-1/2 {50-move rule}")
        ((fide-draw-p state) "1/2-1/2 {Draw per FIDE rules}")
        ((stalematep state) "1/2-1/2 {Stalemate}")
        ((repetitionp state) "1/2-1/2 {Draw by repetition}")
        ((matep state) (if (= (board-ref (State0x88-board state) +turn-store+) +white+)
                           "0-1 {Black mates}"
                           "1-0 {White mates}"))))

(defmethod state->fen ((state State0x88))
  (parse-state state))

(defmethod apply-move ((state State0x88) (move Move0x88))
  (when-let ((new-state (update-state state move)))
    new-state))

(defmethod legal-states ((state State0x88))
  (let (states)
    (loop for move in (pseudo-moves (board-ref (State0x88-board state) +turn-store+)
                                    state)
          do (when move
               (setf states (cons (apply-move state move) states))))
    states))

(defmethod legal-moves ((state State0x88))
  (mapcar #'last-move (legal-states state)))

(defmethod turn ((state State0x88))
  (if (= (board-ref (State0x88-board state) +turn-store+) +white+)
      :white
      :black))

(defmethod last-move ((state State0x88))
  (let* ((board (State0x88-board state))
         (piece (board-ref board (board-ref board +prev-move-to+))))
    (make-move (board-ref board +prev-move-from+)
               (board-ref board +prev-move-to+)
               (if (not (= (board-ref board +prev-piece+) piece))
                   piece
                   0))))

(defmethod perft (state (depth integer))
  (if (zerop depth)
      1
      (apply #'+ (mapcar (lambda (st)
                           (perft st (1- depth)))
                         (when state
                           (legal-states state))))))

(defmethod dynamicp ((state State0x88))
  (= (board-ref (State0x88-board state) +dynamic-store+) 1))

(defmethod evaluate ((state State0x88))
  (heuristic-value (board-ref (State0x88-board state) +turn-store+)
                   (State0x88-white-pieces state)
                   (State0x88-black-pieces state)
                   (check-situation state)))

(defmethod full-moves ((state State0x88))
  (board-ref (State0x88-board state) +full-move-store+))

(defmethod game-end-p ((state State0x88))
  (or (drawp state)
      (matep state)))

(defmethod game-score ((state State0x88))
  (end-score state))

(defun fen->state (fen)
  "Convert given FEN to state representation."
  (update-opponent-check (parse-fen fen (make-State0x88))))

(defmethod copy-state ((state State0x88))
  "Returns copy of given state."
  (make-State0x88 :board (copy-seq (State0x88-board state))
                  :black-pieces (copy-tree (State0x88-black-pieces state))
                  :white-pieces (copy-tree (State0x88-white-pieces state))))
