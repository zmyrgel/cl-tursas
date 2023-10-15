;;;; state0x88.lisp has main 0x88 code.

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
    (cond ((= (Move0x88-from move) #x04)
           (logand castling king-m))
          ((= (Move0x88-from move) #x00)
           (logand castling l-rook-m))
          ((= (Move0x88-from move) #x07)
           (logand castling r-rook-m))
          ((= (Move0x88-to move) #x70)
           (logand castling l-rook-c))
          ((= (Move0x88-to move) #x77)
           (logand castling r-rook-c))
          (t
           castling))))

(defun inc-full-moves! (board)
  "Utility to increase full moves on the board.
   Uses two vector indexes because of the limitation of byte value.
   If full moves get to 127 increase multiplier store and reduce full move
   store to 0. This gets full move count to get high enough."
  (let ((moves (board-ref board +full-move-store+))
        (n-moves (board-ref board +full-move-n-store+)))
    (cond ((= moves 127)
           (fill-square! board +full-move-n-store+ (1+ n-moves))
           (fill-square! board +full-move-store+ 0))
          (t
           (fill-square! board +full-move-store+ (1+ moves))))))

(defun promotion-piece (player move)
  "Helper function to return promotion piece value.
    Reads the promotion piece value from move, defaults to queen."
  (let ((piece (Move0x88-promotion move)))
    (cond ((and (zerop piece) (= player +white+))
           +white-queen+)
          ((and (zerop piece) (= player +black+))
           +black-queen+)
          ((= player +white+)
           piece)
          (t
           (- piece)))))

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
         (indexes (loop for x in piece-list collect (car x)))
         (pieces (loop for x in piece-list collect (cdr x)))
         (piece-count (list-length indexes)))
    (and (<= piece-count 4)
         (or (= piece-count 2)
             (and (= piece-count 3)
                  (loop for p in pieces
                        when (or (= +black-knight+ p)
                                 (= +black-bishop+ p)
                                 (= +white-knight+ p)
                                 (= +white-bishop+ p))
                          do (return t)))
             (and (= piece-count 4)
                  (or (= 2 (list-length (remove-if (lambda (p)
                                                     (= +black-knight+ p))
                                                   pieces)))
                      (= 2 (list-length (remove-if (lambda (p)
                                                     (= +white-knight+ p))
                                                   pieces)))
                      (let ((bishops (remove-if (lambda (i)
                                                  (or (= +black-bishop+ (rest (assoc i piece-list)))
                                                      (= +white-bishop+ (rest (assoc i piece-list)))))
                                                indexes)))
                        (unless (< (list-length bishops) 2)
                          (same-color-p (first (loop for x in bishops collect (first x)))
                                        (second (loop for x in bishops collect (rest x))))))))))))

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
    (unless (threatenedp board (king-index board player) (opponent player))
      state)))

(defun update-castling (state move)
  "Updates states castling value by checking move with current castling value."
  (let* ((board (State0x88-board state))
         (castling (board-ref board +castling-store+))
         (player (board-ref board +turn-store+)))
    (cond ((zerop castling)
           state)
          (t
           (fill-square! board +castling-store+ (calculate-castling player castling move))
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
  "Update the full move counter of given game STATE."
  (let ((board (State0x88-board state)))
    (when (= (board-ref board +turn-store+) +black+)
      (inc-full-moves! board))
    state))

(defun update-opponent-check (state)
  "Update opponent check status bit on the given STATE.
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
  "Update player turn value on the given game STATE."
  (let ((board (State0x88-board state)))
    (fill-square! board +turn-store+ (opponent (board-ref board +turn-store+)))
    state))

(defun update-state (state move)
  "Update game STATE to reflect changes done from executing given MOVE.
If game STATE is not legal, the function will return a nil value,
otherwise the changed STATE is returned."
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
  "Return the game situation of given game STATE, either `opening', `middle' or `end-game'."
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
  ;; a bit weird to say to say game is in check-mate when checkp
  ;; function returns nil but this can happend when generating state
  ;; from fen string and current player is in check/mate. To fix such
  ;; situation and the test here to `or' instead of `and'.
  (or (checkp state)
      (null (legal-states state))))

(defmethod drawp ((state State0x88))
  (or (fifty-move-rule-p state)
      (fide-draw-p state)
      (stalematep state)
      (repetitionp state)))

(defmethod result ((state State0x88))
  (cond ((fifty-move-rule-p state)
         :fifty-move-rule)
        ((fide-draw-p state)
         :fide-draw)
        ((stalematep state)
         :stalemate)
        ((repetitionp state)
         :repetition)
        ((matep state)
         (if (= (board-ref (State0x88-board state) +turn-store+) +white+)
             :mate-for-black
             :mate-for-white))
        (t
         (error "game is still in progress!"))))

(defmethod state->fen ((state State0x88))
  (parse-state state))

(defmethod apply-move ((state State0x88) (move Move0x88))
  (when-let ((new-state (update-state state move)))
    new-state))

(defmethod legal-states ((state State0x88))
  (loop for move in (pseudo-moves (board-ref (State0x88-board state) +turn-store+) state)
        when (and move (apply-move state move))
          collect it))

(defmethod legal-moves ((state State0x88))
  (loop for s in (legal-states state)
        collect (last-move s)))

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

(defmethod perft ((state State0x88) (depth integer))
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

;; XXX: fails if given FEN string is already in check/mate situation
;; for current player.
;; then check bit won't get added
(defun fen->state (fen)
  "Convert given FEN to state representation."
  (make-state fen))

(defmethod copy-state ((state State0x88))
  "Returns copy of given state."
  (make-State0x88 :board (copy-seq (State0x88-board state))
                  :black-pieces (copy-tree (State0x88-black-pieces state))
                  :white-pieces (copy-tree (State0x88-white-pieces state))))
