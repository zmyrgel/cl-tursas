(in-package :tursas.state0x88)

(defconstant +castling-values+ '((8 . #\K)
                                 (4 . #\Q)
                                 (2 . #\k)
                                 (1 . #\q)))

(defun castling->str (castling)
  "Converts internal castling representation to string."
  (let ((result (apply (alexandria:curry #'concatenate 'string)
                       (mapcar (lambda (castling-value)
                                 (when (plusp (logand castling (first castling-value)))
                                   (string (rest castling-value))))
                               +castling-values+))))
    (if (string-equal result "")
        "-"
        result)))

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
  (let ((king (if (= player +white+)
                  +white-king+
                  +black-king+)))
    (labels ((seek (alist)
               (destructuring-bind (index . piece)
                   (first alist)
                 (if (= piece king)
                     index
                     (seek (rest alist))))))
      (seek (alexandria:hash-table-alist (if (= player +white+)
                                             (State0x88-white-pieces state)
                                             (State0x88-black-pieces state)))))))

(defun expand-digits (s chr)
  "Expands digits in given string by that many of given chars."
  (labels ((expander (acc i)
             (if (= i (length s))
                 acc
                 (let* ((e (string (aref s i)))
                        (n (parse-integer e :junk-allowed t)))
                   (if n
                       (expander (concatenate 'string acc (make-string n :initial-element chr)) (1+ i))
                       (expander (concatenate 'string acc e) (1+ i)))))))
    (expander "" 0)))

(defun string-indexed (s)
  "Return an alist of offset / character pairs for string S."
  (mapcar #'cons
          (loop for n below (length s) collect n)
          (map 'list (lambda (c) (coerce c 'character)) s)))

(defun fen-board->0x88board (s)
  "Converts string given in FEN notation to 0x88 board representation."
  (let ((board (init-game-board)))
    (dolist (pair (string-indexed (apply (alexandria:curry #'concatenate 'string)
                                         (mapcar (lambda (row) (concatenate 'string row "EEEEEEEE"))
                                                 (reverse (cl-utilities:split-sequence #\/ (expand-digits s #\E)))))))
      (fill-square! board (car pair) (piece-value (cdr pair))))
    board))

;; XXX: move this
(defun compact-item (x list)
  "Returns a list where each X in LIST is replaced by digit
   character indicating their amount."
  (labels ((f (result items found)
             (cond ((null items) (if (plusp found)
                                     (nreverse (cons found result))
                                     (nreverse result)))
                   ((equalp (first items) x) (f result (rest items) (1+ found)))
                   (t (if (plusp found)
                          (f (cons (digit-char found) result) items 0)
                          (f (cons (first items) result) (rest items) found))))))
    (f nil list 0)))

(defmethod to-string (arg) (string arg))
(defmethod to-string ((arg integer)) (write-to-string arg))

(defun str (&rest args)
  (apply (curry #'concatenate 'string)
         (mapcar #'to-string args)))

(defun make-fen-row (board row)
  "Builds single fen row from given board and row index."
  (apply #'str (compact-item #\E (mapcar (lambda (n)
                                           (piece-name (board-ref board (+ row n))))
                                         (alexandria:iota 8)))))

(defun board->fen-board (board)
  "Convert the given state's board to fen board field."
  (format nil "~{~a~^/~}" (mapcar (alexandria:curry #'make-fen-row board)
                                  '(#x70 #x60 #x50 #x40 #x30 #x20 #x10 #x0))))

(defun add-pieces (state)
  "Adds all pieces from board to piece-map."
  (loop :for i :below #x77
        :do (when (board-index-p i)
              (let ((piece (board-ref (State0x88-board state) i)))
                (cond ((black-piece-p piece)
                       (setf (gethash i (State0x88-black-pieces state)) piece))
                      ((white-piece-p piece)
                       (setf (gethash i (State0x88-white-pieces state)) piece))
                      (t nil))))))

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
  (let ((n-moves (/ moves 128)))
    (fill-square! board +full-move-n-store+ n-moves)
    (fill-square! board +full-move-store+ (- moves (* n-moves 128)))))

(defun parse-fen (s state)
  "Parses information from given FEN and applies it to given state."
  (destructuring-bind (board turn cast en-pass half full)
      (cl-utilities:split-sequence #\space s)
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
    (reduce (lambda (m o)
              (concatenate 'string m " " o))
            (list (board->fen-board board)
                  (if (= (board-ref board +turn-store+) +white+) "w" "b")
                  (castling->str (board-ref board +castling-store+))
                  (let ((en-passant (board-ref board +en-passant-store+)))
                    (if (= en-passant -1)
                        "-"
                        (index->coord en-passant)))
                  (board-ref board +half-move-store+)
                  (+ (* (board-ref board +full-move-n-store+) 127)
                     (board-ref board +full-move-store+))))))
