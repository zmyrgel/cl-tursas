;;;; eval0x88.lisp provides board evaluation functions.

(in-package :tursas.state0x88)

(define-constant +pawn-value+ 10)
(define-constant +bishop-value+ 30)
(define-constant +knight-value+ 30)
(define-constant +rook-value+ 50)
(define-constant +queen-value+ 90)
(define-constant +king-value+ 99999)

(eval-when (:compile-toplevel :load-toplevel)
  (defun group (list n)
    "Group items in list to lists of n length."
    (declare (type (integer 0 10) n))
    (when (zerop n) (error "Groups of zero are not allowed."))
    (labels ((rec (list acc)
               (let ((rest (nthcdr n list)))
                 (if (consp rest)
                     (rec rest (cons (subseq list 0 n) acc))
                     (nreverse (cons list acc))))))
      (when list
        (rec list nil))))

  (defun make-table (score-list)
    "Utility to make full 0x88 vector board out of score list"
    (make-array 128
                :element-type 'board-value
                :initial-contents (loop for row in (group score-list 8)
                                        append (append row (make-list 8 :initial-element 0))))))

(define-constant +white-pawn-table+
    (make-table '(0   0   0   0   0   0   0   0
                  5   5   5   0   0   5   5   5
                  0   0   5  15  15   5   0   0
                  5   5  10  15  15  10   5   5
                  0   0   5  10  10   5   0   0
                  0   0   0   0   0   0   0   0
                  0   0   0   0   0   0   0   0
                  0   0   0   0   0   0   0   0))
  :test #'equalp)
(define-constant +black-pawn-table+ (reverse +white-pawn-table+)
  :test #'equalp)

(define-constant +white-rook-table+
    (make-table '(5  0  5  0  0  5  0  5
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0))
  :test #'equalp)
(define-constant +black-rook-table+ (reverse +white-rook-table+)
  :test #'equalp)

(define-constant +white-rook-end-table+
    (make-table '(0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0))
  :test #'equalp)
(define-constant +black-rook-end-table+ (reverse +white-rook-end-table+)
  :test #'equalp)

(define-constant +white-knight-table+
    (make-table '(-10 -5 -5  -5  -5  -5  -5 -10
                  -5  -5  0   5   5   0  -5 -5
                  -5   5  5   10  10  5   5 -5
                  -5   0  10  10  10  10  0 -5
                  -5   0  10  10  10  10  0 -5
                  -5   5  5   10  10  10  5 -5
                  -5  -5  0   5   5   0  -5 -5
                  -10 -5  -5  -5  -5  -5 -5 -10))
  :test #'equalp)
(define-constant +black-knight-table+ (reverse +white-knight-table+)
  :test #'equalp)

(define-constant +white-bishop-table+
    (make-table '(-15 -10 -10 -10 -10 -10 -10 -15
                  -10   0   0   0   0   0   0 -10
                  -10   0   5  10  10   5   0 -10
                  -10   5   5  10  10   5   5 -10
                  -10   0  10  10  10  10   0 -10
                  -10  10  10  10  10  10  10 -10
                  -10   5   0   0   0   0   5 -10
                  -15 -10 -10 -10 -10 -10 -10 -15))
  :test #'equalp)
(define-constant +black-bishop-table+ (reverse +white-bishop-table+)
  :test #'equalp)

(define-constant +white-king-table+
    (make-table '(  0  -5   5   0   0  -5   5   0
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10))
  :test #'equalp)
(define-constant +black-king-table+ (reverse +white-king-table+)
  :test #'equalp)

(define-constant +white-king-end-game-table+
    (make-table '(-15 -10 -10 -10 -10 -10 -10 -15
                  -10 -10  -5   0   0  -5 -10 -10
                  -10  -5   5  10  10   5  -5 -10
                  -10  -5  10  15  15  10  -5 -10
                  -10  -5  10  15  15  10  -5 -10
                  -10  -5   5  10  10   5  -5 -10
                  -10 -10  -5   0   0  -5 -10 -10
                  -15 -10 -10 -10 -10 -10 -10 -15))
  :test #'equalp)
(define-constant +black-king-end-game-table+ (reverse +white-king-end-game-table+)
  :test #'equalp)

(defun material-value (piece)
  "Gives material value for given piece."
  (cond ((or (= piece +white-pawn+) (= piece +black-pawn+))
         +pawn-value+)
        ((or (= piece +white-knight+) (= piece +black-knight+))
         +knight-value+)
        ((or (= piece +white-bishop+) (= piece +black-bishop+))
         +bishop-value+)
        ((or (= piece +white-rook+) (= piece +black-rook+))
         +rook-value+)
        ((or (= piece +white-queen+) (= piece +black-queen+))
         +queen-value+)
        ((or (= piece +white-king+) (= piece +black-king+))
         +king-value+)
        (t 0)))

(defun index-score (piece index game-situation)
  "Checks piece-specific index score"
  (cond ((= piece +white-pawn+)
         (board-ref +white-pawn-table+ index))
        ((= piece +black-pawn+)
         (board-ref +black-pawn-table+ index))
        ((= piece +white-knight+)
         (board-ref +white-knight-table+ index))
        ((= piece +black-knight+)
         (board-ref +black-knight-table+ index))
        ((= piece +white-bishop+)
         (board-ref +white-bishop-table+ index))
        ((= piece +black-bishop+)
         (board-ref +black-bishop-table+ index))
        ((= piece +white-rook+)
         (board-ref +white-rook-table+ index))
        ((= piece +black-rook+)
         (board-ref +black-rook-table+ index))
        ((= piece +white-king+)
         (if (= game-situation +end-game+)
             (board-ref +white-king-end-game-table+ index)
             (board-ref +white-king-table+ index)))
        ((= piece +black-king+)
         (if (= game-situation +end-game+)
             (board-ref +black-king-end-game-table+ index)
             (board-ref +black-king-table+ index)))
        (t
         0)))

(defun score (pieces situation)
  "Calculates score for side."
  (loop for (index . piece) in pieces
        summing (+ (material-value piece)
                   (index-score piece index situation))))

(defun heuristic-value (player whites blacks situation)
  "Calculates heuristic value for given state."
  (multiple-value-bind (player-pieces enemy-pieces)
      (if (= player +white+)
          (values whites blacks)
          (values blacks whites))
    (+ (score player-pieces situation)
       (- (score enemy-pieces situation)))))

(defun end-score (state)
  "Return the final score of given state."
  (if (matep state)
      (- +king-value+)
      0))
