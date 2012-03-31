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

(in-package :tursas)

(define-constant +cecp-supported-features+ '((ping . 1)
                                             (setboard . 1)
                                             (playother . 1)
                                             (san . 0)
                                             (usermove . 0)
                                             (time . 0)
                                             (draw . 1)
                                             (sigint . 0)
                                             (sigterm . 0)
                                             (reuse . 0)
                                             (analyse . 0)
                                             (myname . "\"Tursas 0.2\"")
                                             (variants . "\"normal\"")
                                             (colors . 0)
                                             (ics . 0)
                                             (name . 0)
                                             (pause . 0)
                                             (nps . 0)
                                             (debug . 0)
                                             (memory . 0)
                                             (smp . 0)
                                             (done . 1))
  :test 'equal)

(define-constant +cecp-usage+
    '(""
      "Available Cecp commands are:"
      "protover N - change engine to use protocol version N"
      "accepted - Accept last feature"
      "rejected - Reject last feature"
      "new - Sets the board to the chess starting position. Set White on move. Leave force mode and set the engine to play Black."
      "variant VARIANT - change to use VARIANT rules. Only 'normal' supported"
      "force - Disable engine AI"
      "go - Enable engine AI"
      ;;"playother - Tell AI to switch sides"
      ;;"level MPS BASE INC - set time controls"
      ;;"st TIME - set time controls"
      "sd DEPTH - set search depth to DEPTH"
      ;;"nps NODE_RATE - search only NODE_RATE nodes"
      ;;"time N - set the engine clock to N centiseconds"
      ;;"otim N - set the opponents clock"
      "usermove MOVE - make given MOVE if legal"
      "MOVE - make given MOVE if legal"
      ;;"? - Tell Engine to stop thinking and make its move now"
      "ping N - Pings the engine for pong reply"
      "draw - offer draw to engine"
      "result RESULT {COMMENTS} - give the game RESULT to engine, discarded currently"
      "setboard FEN - Set the game board to given FEN."
      "hint - prompt move hint from engine"
      ;;"bk - use book"
      "undo - tell engine to undo last move"
      "remove - tell engine to undo last two moves"
      ;;"hard - tell engine to ponder during players turn"
      ;;"easy - tell engine to ponder only during its turn"
      ;;"post - tell engine to send ponder output"
      ;;"nopost - tell engine not to send ponder output"
      ;;"analyse - tell engine to engage analyse mode"
      "name X - tell engine its opponents name"
      "rating - ask engine its rating"
      ;;"ics - tell engine its engaging in ICS game"
      "computer - tell engine that its playing against cpu"
      ;;"pause - pause all actions"
      ;;"resume - resume all paused actions"
      ;;"memory N - specify how much engine can use memory"
      ;;"cores N - tell engine how many cpu cores it can use"
      ;;"egtpath PATH - tell engine to use end-game tables from PATH"
      "option NAME[=VALUE] - tell engine to use new option")
  :test 'equal)

(defparameter *protocol* 'general)
(defparameter *game-state* nil)
(defparameter *game-options* '((:depth-limit . 4)
                               (:ai-mode . nil)
                               (:cecp-protocol-version . 2)
                               (:debug . nil)
                               (:ponder . nil)
                               (:ponder-output . nil)))

(defun current-game-state ()
  "Utility to return current game state or nil."
  (first *game-state*))

(defun set-option! (key value)
  "Sets game option of given key to value."
  (setf (assoc key *game-options*) value))

(defun get-option (option)
  "Returns value of given game option"
  (assoc option *game-options*))

(defun save-game ()
  "Saves the current game by writing game-state to file."
  (with-open-file (s "saved-game.txt" :direction :output)
    (princ *game-state* s)))

(defparameter *search-fn*
  (lambda (state)
    (alpha-beta most-negative-fixnum most-positive-fixnum (get-option :depth-limit) #'evaluate state)))
(defparameter *shallow-search-fn*
  (lambda (state)
    (alpha-beta most-negative-fixnum most-positive-fixnum 2 #'evaluate state)))

(defun load-game ()
  "Loads the game-state from file to resume the previous game."
  (with-open-file (stream "saved-game.txt")
    (loop for form = (read stream nil 'eof)
          until (eq form 'eof)
          do (setf *game-state* form))))

(defun get-protocol ()
  "Returns currently active reader protocol"
  *protocol*)

(defun set-protocol! (prot)
  "Sets the currently active reader protocol"
  (setf *protocol* prot))

(defun tursas-cmd (msg f &rest args)
  "Wrapper for commands which use current game state."
  (if (null (current-game-state))
      (concatenate 'string "Error (" msg ")")
      (apply f (current-game-state) args)))

(defun add-game-state! (new-state)
  "Adds given state to game state."
  (setf *game-state* (cons new-state *game-state*))
  t)

(defun display-board (state)
  "Displays the current chess board in ASCII."
  (concatenate 'string
               (fen->ascii (state->fen state))
               (if (eq (turn state) :white) "  WHITE" "  BLACK")
               " TO MOVE"
               (cond ((matep state) (if (eq (turn state) :white)
                                        ", WHITE IN MATE!"
                                        ", BLACK IN MATE!"))
                     ((checkp state) (if (eq (turn state) :white)
                                         ", WHITE IN CHECK!"
                                         ", BLACK IN CHECK!")))))

(defun display-fen (state)
  "Display FEN of currect game state."
  (state->fen state))

(defun display-perft (state depth)
  "Display Perft of given depth."
  (perft state depth))

(defun list-moves (state)
  "List all available moves from currect state."
  (mapcar #'move->coord (legal-moves state)))

(defun get-score (state)
  "Calculates state's score by checking child states
   to certain depth using alpha-beta algorithm."
  (funcall *search-fn* state))

(defun eval-current-state (state)
  "Evaluates the current state and returns its score."
  (concatenate 'string (evaluate state)))

(defun get-hint (state)
  "Evaluates all states and chooses one from top five moves at random."
  (multiple-value-bind (score best-state)
      (funcall *shallow-search-fn* state)
    (declare (ignore score))
    (move->coord (last-move best-state))))

(defun fenp (s)
  "Predicate to check if given string S is a valid FEN string."
  (when (scan "^[1-8prnbqkPRNBQK/]{15,71}\\s[wb]{1}\\s[KQkq-]{1,4}\\s[a-h1-8-]{1,2}\\s\\d+\\s\\d+$" s)
    t))

(defun set-game! (s)
  "Sets game to given FEN state.
   Command also supports some pre-defined states, mainly for debugging."
  (let ((startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        (check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")
        (cast-fen "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")
        (prom-fen "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")
        (mate-fen "3brr2/7b/8/2pN3Q/2p2k2/5P2/4P1KR/2N2RB1 b - - 1 18")
        (mate-1-fen "3brr2/2N4b/8/2p4Q/2p2k2/5P2/4P1KR/2N2RB1 w - - 1 17")
        (en-fen "rnbqkb1r/pppppppp/7n/P7/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2"))
    (if-let ((fen (cond ((equal s "startpos") startpos)
                        ((equal s "check") check-fen)
                        ((equal s "cast") cast-fen)
                        ((equal s "prom") prom-fen)
                        ((equal s "mate") mate-fen)
                        ((equal s "bmate") mate-1-fen)
                        ((equal s "en") en-fen)
                        (t (when (fenp s)
                             s)))))
      (add-game-state! (fen->state fen))
      (concatenate 'string "Error (Invalid setboard command): " s))))

(defun toggle-option! (option)
  "Toggles the value of given boolean game option."
  (set-option! option (not (get-option option))))

(defun make-ai-move! (state)
  "Tell engine to make an move in current game state."
  (multiple-value-bind (score best-state)
      (funcall *search-fn* state)
    (declare (ignore score))
    (add-game-state! best-state))
  (concatenate 'string "move "
               (move->coord (last-move (current-game-state)))))

(defun make-human-move! (state s)
  "If given string represents chess move, apply it to current game."
  (when (and (user-move-p s)
             (allowedp state (coord->move s)))
    (when-let ((new-state (apply-move state (coord->move s))))
      (add-game-state! new-state))))

(defun game-result (state)
  "Returns string representation for game result."
  (case (result state)
    (:FIFTY-MOVE-RULE "1/2-1/2 {50-move rule}")
    (:FIDE-DRAW "1/2-1/2 {Draw per FIDE rules}")
    (:STALEMATE "1/2-1/2 {Stalemate}")
    (:REPETITION "1/2-1/2 {Draw by repetition}")
    (:MATE-FOR-BLACK "0-1 {Black mates}")
    (:MATE-FOR-WHITE "1-0 {White mates}")
    (otherwise nil)))

(defun user-move (state s)
  "Helper function to handle user and ai moves.
   State arg included to avoid user moves when game is not set."
  (if (null (make-human-move! state s))
      (concatenate 'string "Illegal move: " s)
      (if (game-end-p (current-game-state))
          (game-result (current-game-state))
          (when (get-option :ai-mode)
            (let ((move (make-ai-move! (current-game-state))))
              (if (game-end-p (current-game-state))
                  (format nil "~a~%~a" move (game-result (current-game-state)))
                  move))))))

(defun undo-move! (&optional n)
  "Undo last move or if N given, N last moves."
  (setf *game-state* (if n
                         (nthcdr *game-state* n)
                         (rest *game-state*))))

(defun list-cecp-supported-features ()
  "Prints the default features of the engine."
  (mapcar (lambda (option)
            (format nil "feature ~a=~a" option (rest (assoc option +cecp-supported-features+))))
          (mapcar #'car +cecp-supported-features+)))

(defun cecp-draw (state)
  "Offer draw to opponent."
  (when (get-option 'ai-mode)
    (concatenate 'string "1/2-1/2 {" (if (eq (turn state) :white)
                                         "WHITE"
                                         "BLACK")
                 " offered a draw!}")))

(defun cecp-parse-option (option)
  "Wrapper to parse options from string and set them."
  (destructuring-bind (key &optional value)
      (split-sequence #\= (string option))
    (set-option! (intern key :keyword) (if value value t))))

(defun unsupported-command (cmd)
  "Utility to return error message for unsupported commands."
  (concatenate 'string "Error (Unsupported command): " cmd))

(defun set-variant (variant)
  (if (string= variant "normal")
      (set-option! :variant "normal")
      (concatenate 'string "Error (unsupported variant given): " variant)))

(defun user-move-p (s)
  "Predicate to check if given string S represents user move."
  (scan "^[a-h]{1}[1-8]{1}[a-h]{1}[1-8]{1}[rnbq]?$" s))

(defun process-cecp-command (cmd)
  "Processes command in cecp mode."
  (cond ((register-groups-bind ((#'parse-integer arg))
             ("^protover\\s(\\d)$" cmd)
           (progn (set-option! :cecp-protocol-version arg)
                  (list-cecp-supported-features))))
        ((scan "^accepted$" cmd) nil)
        ((scan "^rejected$" cmd) nil)
        ((scan "^random$" cmd) nil)
        ((scan "^new$" cmd) (progn (set-game! "startpos")
                                         (set-option! :ai-mode t)))
        ((register-groups-bind ((#'parse-integer arg))
             ("^variant\\s(\\w+)$" cmd)
           (set-variant arg)))
        ((scan "^force$" cmd) (set-option! :ai-mode nil))
        ((scan "^go$" cmd) (set-option! :ai-mode t))
        ((scan "^playother$" cmd) (unsupported-command cmd))
        ((scan "^level\\s\\d+\\s[0-9:]+\\s\\d+$" cmd) (unsupported-command cmd))
        ((scan "^st\\s\\d+$" cmd) (unsupported-command cmd))
        ((register-groups-bind ((#'parse-integer arg))
             ("^sd\\s(\\d+)$"cmd)
           (set-option! :depth-limit arg)))
        ((scan "^nps\\s\\d+$" cmd) (unsupported-command cmd))
        ((scan "^time\\s\\d+$" cmd) (unsupported-command cmd))
        ((scan "^otim\\s\\d+$" cmd) (unsupported-command cmd))
        ((register-groups-bind (arg)
             ("^usermove\\s([a-h]{1}[1-8]{1}[a-h]{1}[1-8]{1}[rnbq]?)$" cmd)
           (tursas-cmd "Can't make move in a empty board!" #'user-move arg)))
        ((user-move-p cmd) (tursas-cmd "Can't make move in a empty board!" #'user-move cmd))
        ((scan "^\\?$" cmd) (unsupported-command cmd))
        ((register-groups-bind ((#'parse-integer arg))
             ("^ping\\s(\\d+)$" cmd)
           (concatenate 'string "pong " arg)))
        ((scan "^draw$" cmd) (tursas-cmd "Can't offer draw to empty board!" #'cecp-draw))
        ((scan "^result\\s(1/2-1/2\\s\{.+\}|1-0\\s\{.+\}|0-1\\s\{.+\}|\\*)$" cmd) nil)
        ((register-groups-bind (arg)
             ("^setboard\\s(.+)$" cmd)
           (set-game! arg)))
        ((scan "^hint$" cmd) (tursas-cmd "Can't print hint from a empty board!" #'get-hint))
        ((scan "^bk$" cmd) (unsupported-command cmd))
        ((scan "^undo$" cmd) (undo-move!))
        ((scan "^remove$" cmd) (undo-move! 2))
        ((scan "^hard$" cmd) (unsupported-command cmd))
        ((scan "^easy$" cmd) (unsupported-command cmd))
        ((scan "^post$" cmd) (unsupported-command cmd))
        ((scan "^nopost$" cmd) (unsupported-command cmd))
        ((scan "^analyse$" cmd) (unsupported-command cmd))
        ((register-groups-bind (arg)
             ("^name\\s(\\w+)$" cmd)
           (set-option! :opponent-name arg)))
        ((scan "^rating$" cmd) 100)
        ((scan "^ics$" cmd) (unsupported-command cmd))
        ((scan "^computer$" cmd) (set-option! :opponent :cpu))
        ((scan "^pause$" cmd) (unsupported-command cmd))
        ((scan "^resume$" cmd) (unsupported-command cmd))
        ((scan "^memory\\s\\d+$" cmd) (unsupported-command cmd))
        ((scan "^cores\\s\\d+$" cmd) (unsupported-command cmd))
        ((scan "^egtpath\\s[\\w\\/]+$" cmd) (unsupported-command cmd))
        ((register-groups-bind (args)
             ("^option\\s\\w=\".+\"|\\d+$" cmd)
           (cecp-parse-option args)))
        (t (concatenate 'string "Error (Invalid command): " cmd))))

(defun usage ()
  "Prints the available commands of the repl."
  (concatenate 'list
               '("Available general commands:"
                 "help - display this help"
                 "load - load the last saved game from file"
                 "save - store the current game to file"
                 "bd - display the board on the screen"
                 "fd - display current game state in FEN"
                 "lm - print a list of all available moves"
                 ;;"gs - calculates score for the current game state"
                 ;;"es - evaluates current game state"
                 ;;"pf n - calculate perft score to depth of n"
                 "xboard - enable CECP mode"
                 "quit - quit the Tursas engine")
               (when (eq (get-protocol) :cecp)
                 +cecp-usage+)))

(defun process-command (cmd)
  "Processes command given by user."
  (cond ((scan "^$" cmd) nil)
        ((scan "^help$" cmd) (usage))
        ((scan "^load$" cmd) (load-game))
        ((scan "^save$" cmd) (save-game))
        ((scan "^bd$" cmd) (tursas-cmd "Can't print empty board!" #'display-board))
        ((scan "^fd$" cmd) (tursas-cmd "Can't display FEN for empty state." #'display-fen))
        ((scan "^lm$" cmd) (tursas-cmd "Can't list moves from empty state." #'list-moves))
        ((scan "^gs$" cmd) (tursas-cmd "Can't calculate score from empty state." #'get-score))
        ((scan "^cp$" cmd) (progn (tursas-cmd "Can't make AI move on empty board!" #'make-ai-move!)
                                        (tursas-cmd "Can't print empty board!" #'display-board)))
        ((scan "^es$" cmd) (tursas-cmd "Can't eval empty game state!" #'eval-current-state))
        ((register-groups-bind ((#'parse-integer arg))
             ("^pf\\s(\\d+)$" cmd)
           (tursas-cmd "Can't calculate perft from empty game-state!"
                       #'display-perft arg)))
        ((scan "^xboard$" cmd) (set-protocol! :cecp))
        (t (if (eq (get-protocol) :cecp)
               (process-cecp-command cmd)
               (concatenate 'string "Error (Invalid command): " cmd)))))
