(in-package :tursas.state0x88)

(defun calculate-white-castling (castling move)
  "Utility to calculate new castling value after white players move.
   Castling value is updated if either our king or rook moves
   or opponents rook gets captured.
   Castling value is kept as a number and is operated at bit level.
   Castling value is composesd as: K = 8, Q = 4, k = 2, q = 1"
  (cond ((= (Move0x88-from move) #x04) (logand castling 3))
        ((= (Move0x88-from move) #x00) (logand castling 11))
        ((= (Move0x88-from move) #x07) (logand castling 7))
        ((= (Move0x88-to move) #x70) (logand castling 14))
        ((= (Move0x88-to move) #x77) (logand castling 13))
        (t castling)))

(defun calculate-black-castling (castling move)
  "Utility to calculate new castling value after black players move.
   Castling value is updated if either our king or rook moves
   or opponents rook gets captured.
   Castling value is kept as a number and is operated at bit level.
   Castling value is composesd as: K = 8, Q = 4, k = 2, q = 1"
  (cond ((= (Move0x88-from move) #x74) (logand castling 12))
        ((= (Move0x88-from move) #x70) (logand castling 14))
        ((= (Move0x88-from move) #x77) (logand castling 13))
        ((= (Move0x88-to move) #x00) (logand castling 11))
        ((= (Move0x88-to move) #x07) (logand castling 7))
        (t castling)))

(defun calculate-en-passant (player piece west-piece east-piece move)
  "Utility to calculate new en-passant index value.
   If pawn moves two steps next to opponents pawn, return en-passant
   value as board index just behind moved pawn, otherwise -1."
  (let ((opp-pawn (if (= player +white+) +black-pawn+ +white-pawn+)))
    (if (and (or (= piece +white-pawn+)
                 (= piece +black-pawn+))
             (= (abs (- (Move0x88-to move) (Move0x88-from move))) #x20)
             (or (= opp-pawn west-piece)
                 (= opp-pawn east-piece)))
        (/ (+ (Move0x88-to move) (Move0x88-from move)) 2)
        -1)))

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

(defun merge-hash-tables (&rest tables)
  "Returns new hash table with all the keys and values from given tables."
  (let ((union (make-hash-table
                :test (first
                       (sort (mapcar #'hash-table-test tables) #'>
                             :key (lambda (test)
                                    (ecase test
                                      (eq 0)
                                      (eql 1)
                                      (equal 2)
                                      (equalp 3)))))
           :size (reduce #'max (mapcar #'hash-table-size tables)))))
    (dolist (table tables)
      (maphash (lambda (key val) (setf (gethash key union) val)) table))
    union))

(defun fide-draw-p (state)
  "Checks if state is draw according to FIDE rules:
   - Both sides have only king piece.
   - One side has king and bishop or knight vs. others king
   - One sides king and two knights agains others bare king
   - Both sides have only bishop of same color besides kings"
  (let* ((piece-map (merge-hash-tables (State0x88-white-pieces state)
                                       (State0x88-black-pieces state)))
         (indexes (alexandria:hash-table-keys piece-map))
         (pieces (alexandria:hash-table-values piece-map))
         (piece-count (list-length indexes)))
    (and (<= piece-count 4)
         (or (= piece-count 2)
             (and (= piece-count 3)
                  (some (lambda (piece)
                          (or (= +black-knight+ piece)
                              (= +black-bishop+ piece)
                              (= +white-knight+ piece)
                              (= +white-bishop+ piece)))
                        pieces))
             (and (= piece-count 4)
                  (or (= 2 (list-length (remove-if (lambda (piece)
                                                     (= +black-knight+ piece))
                                                   pieces)))
                      (= 2 (list-length (remove-if (lambda (piece)
                                                     (= +white-knight+ piece)) pieces)))
                      (let ((bishops (remove-if (lambda (index)
                                                  (or (= +black-bishop+ (gethash index piece-map))
                                                      (= +white-bishop+ (gethash index piece-map))))
                                                indexes)))

                        (when (not (< (list-length bishops) 2))
                          (same-color-p (first (alexandria:hash-table-keys bishops))
                                        (second (alexandria:hash-table-keys bishops)))))))))))

(defun repetitionp (state)
  "Predicate to see if game is draw by repetition.
   Stub function to be filled later."
  nil)

(defun update-move (state move)
  "Update the previous move of board.
   Stores previous move to 'off-board' locations"
  (when (not (null state))
    (let ((board (State0x88-board state)))
      (fill-square! board +prev-move-from+ (Move0x88-from move))
      (fill-square! board +prev-move-to+ (Move0x88-to move))
      (fill-square! board +prev-piece+ (board-ref board (Move0x88-from move)))
      state)))

(defun pawn-or-capture-move-p (board move)
  "Predicate to see if move was pawn move or a capture"
  (or (= (board-ref board (Move0x88-from move)) +white-pawn+)
      (= (board-ref board (Move0x88-from move)) +black-pawn+)
      (not (= (board-ref board (Move0x88-to move)) +empty-square+))))

(defun update-half-moves (state move)
  "Increases half move count on board unless the move
   was pawn or a capture move."
  (when (not (null state))
    (let ((board (State0x88-board state)))
      (fill-square! board +half-move-store+
                    (if (pawn-or-capture-move-p board move)
                        0
                        (1+ (board-ref board +half-move-store+))))
      state)))

(defun update-board (state move)
  "Returns state with new board after applying move to state."
  (when (not (null state))
    (let* ((board (State0x88-board state))
           (player (board-ref board +turn-store+))
           (moving-piece (board-ref board (Move0x88-from move))))
      (cond ((promotionp moving-piece move)
             (remove-piece! state (Move0x88-from move))
             (add-piece! state (Move0x88-to move) (promotion-piece player move)))
            ((castlingp moving-piece move)
             (move-castling-pieces! player state move (if (= (column (Move0x88-to move)) 2)
                                                          +queen-side+
                                                          +king-side+)))
            ((en-passant-p board moving-piece move)
             (move-piece! state move)
             (remove-piece! state (+ (Move0x88-to move)
                                     (if (= player +white+)
                                         +south+
                                         +north+))))
            (t (move-piece! state move)))
      state)))

(defun update-player-check (state)
  "Checks that players move won't leave the players king in check."
  (when (not (null state))
    (let* ((board (State0x88-board state))
           (player (board-ref board +turn-store+)))
      (when (not (threatenedp board (king-index board player) (opponent player)))
        state))))

(defun update-castling (state move)
  "Updates states castling value by checking move with current castling value."
  (when (not (null state))
    (let* ((board (State0x88-board state))
           (castling (board-ref board +castling-store+)))
      (if (zerop castling)
          state
          (progn (fill-square! board +castling-store+ (if (= (board-ref board +turn-store+) +white+)
                                                          (calculate-white-castling castling move)
                                                          (calculate-black-castling castling move)))
                 state)))))

(defun update-en-passant (state move)
  "Associates new en-passant value with given state based on the move."
  (when (not (null state))
    (let ((board (State0x88-board state)))
      (fill-square! board +en-passant-store+
                    (calculate-en-passant (board-ref board +turn-store+)
                                          (board-ref board (Move0x88-to move))
                                          (board-ref board (+ (Move0x88-to move) +west+))
                                          (board-ref board (+ (Move0x88-to move) +east+))
                                          move))
      state)))

(defun update-full-moves (state)
  "Updates full move count on board."
  (when (not (null state))
    (let ((board (State0x88-board state)))
      (when (= (board-ref board +turn-store+) +black+)
        (inc-full-moves! board))
      state)))

(defun update-opponent-check (state)
  "Updates opponents check status bit on the state.
   Enables check bit in state if opponents king is threatened."
  (when (not (null state))
    (let* ((board (State0x88-board state))
           (player (board-ref board +turn-store+)))
      (fill-square! board +check-store+
                    (if (threatenedp board
                                     (king-index board (opponent player))
                                     player)
                        1
                        0))
      state)))

(defun update-turn (state)
  "Updates player turn value on board."
  (when (not (null state))
    (let ((board (State0x88-board state)))
      (fill-square! board +turn-store+ (opponent (board-ref board +turn-store+)))
      state)))

(defun update-state (state move)
  "Updates game state to reflect changes from move.
   If game state is not legal, will return a nil value."
  (when (not (null state))
    (let ((new-state (copy-state state)))
      (update-move new-state move)
      (update-half-moves new-state move)
      (update-board new-state move)
      (update-player-check new-state)
      (update-castling new-state move)
      (update-en-passant new-state move)
      (update-full-moves new-state)
      (update-opponent-check new-state)
      (update-turn new-state)
      new-state)))

(defun check-situation (state)
  "Checks which situation, opening, middle or end-game the game is."
  (let ((pieces (merge-hash-tables (State0x88-white-pieces state)
                                   (State0x88-black-pieces state))))
    (cond ((< (list-length (hash-table-keys pieces)) 15) +end-game+)
          ((> (board-ref (State0x88-board state) +full-move-store+) 10) +middle-game+)
          (t +opening-game+))))

(defstruct State0x88
  (board (init-game-board) :type simple-vector)
  (black-pieces (make-hash-table) :type hash-table)
  (white-pieces (make-hash-table) :type hash-table))

(defmethod allowedp ((state State0x88) move)
  (allowed-move-p state move))

(defmethod occupiedp ((state State0x88) index)
  (board-occupied-p (State0x88-board state) index))

(defmethod blackp ((state State0x88) index)
  (occupied-by-p (State0x88-board state) index +black+))

(defmethod whitep ((state State0x88) index)
  (occupied-by-p (State0x88-board state) index +white+))

(defmethod checkp ((state State0x88))
  (= (board-ref (State0x88-board state) +check-store+) 1))

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
  (alexandria:when-let ((new-state (update-state state move)))
    new-state))

(defmethod legal-states ((state State0x88))
  (delete nil (mapcar (alexandria:curry #'apply-move state)
                      (pseudo-moves (board-ref (State0x88-board state) +turn-store+) state))))

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

(defmethod perft ((state State0x88) depth)
  (if (zerop depth)
      1
      (apply #'+ (mapcar (lambda (st)
                           (perft st (1- depth)))
                         (legal-states state)))))

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
                  :black-pieces (copy-hash-table (State0x88-black-pieces state))
                  :white-pieces (copy-hash-table (State0x88-white-pieces state))))
