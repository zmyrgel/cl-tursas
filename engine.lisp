(in-package :tursas)

(defconstant +cecp-supported-features+ '((ping . 1)
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
                                         (myname . "\"Tursas 0.1\"")
                                         (variants . "\"normal\"")
                                         (colors . 0)
                                         (ics . 0)
                                         (name . 0)
                                         (pause . 0)
                                         (nps . 0)
                                         (debug . 0)
                                         (memory . 0)
                                         (smp . 0)
                                         (done . 1)))

(defconstant +cecp-usage+
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
    "option NAME[=VALUE] - tell engine to use new option"))

(defparameter *protocol* 'general)
(defparameter *game-state* nil)
(defparameter *game-options* '((depth-limit . 4)
                               (ai-mode . nil)
                               (cecp-protocol-version . 2)
                               (debug . nil)
                               (ponder . nil)
                               (ponder-output . nil)))

(defparameter *allowed-commands*
  '(protover accepther rejected new
    variant force go sd usermove ping
    draw setboard hint undo remove
    name rating computer option
    help load save bd fd lm gs es pf xboard quit))

(defun quit ()
  "Function to handle closing the engine."
  (quit))

(defun current-game-state ()
  "Utility to return current game state or nil."
  (first *game-state*))

(defun set-option! (key value)
  "Sets game option of given key to value."
  (rplacd (assoc key *game-options*) value))

(defun get-option (option)
  "Returns value of given game option"
  (rest (assoc option *game-options*)))

(defun save-game ()
  "Saves the current game by writing game-state to file."
  (with-open-file (s "saved-game.txt" :direction :output)
    (princ *game-state* s)))

(defparameter *search-fn* (alexandria:curry #'alpha-beta (get-option 'depth-limit) #'evaluate))
(defparameter *shallow-search-fn* (alexandria:curry #'alpha-beta 2 #'evaluate))

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
  (cons new-state *game-state*))

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
  (time (perft state depth)))

(defun list-moves (state)
  "List all available moves from currect state."
  (mapcar #'move->coord (legal-moves state)))

(defun get-score (state)
  "Calculates state's score by checking child states
   to certain depth using alpha-beta algorithm."
  (concatenate 'string (first (funcall *search-fn* state))))

(defun eval-current-state (state)
  "Evaluates the current state and returns its score."
  (concatenate 'string (evaluate state)))

(defun get-hint (state)
  "Evaluates all states and chooses one from top five moves at random."
  (move->coord (last-move (second (funcall *shallow-search-fn* state)))))

(defun fenp (s)
  "Predicate to check if given string S is a valid FEN string."
  (when (ppcre:scan "^[1-8prnbqkPRNBQK/]{15,71}\\s[wb]{1}\\s[KQkq-]{1,4}\\s[a-h1-8-]{1,2}\\s\\d+\\s\\d+$" s)
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
    (alexandria:if-let ((fen (cond ((equal s "startpos") startpos)
                                   ((equal s "check") check-fen)
                                   ((equal s "cast") cast-fen)
                                   ((equal s "prom") prom-fen)
                                   ((equal s "mate") mate-fen)
                                   ((equal s "bmate") mate-1-fen)
                                   ((equal s "en") en-fen)
                                   (t (when (fenp s)
                                        s)))))
      (add-game-state! (tursas.state0x88:fen->state fen))
      (concatenate 'string "Error (Invalid setboard command): " s))))

(defun toggle-option! (option)
  "Toggles the value of given boolean game option."
  (set-option! option (not (get-option option))))

(defun make-ai-move! (state)
  "Tell engine to make an move in current game state."
  (add-game-state! (second (funcall *search-fn* state)))
  (concatenate 'string "move "
               (move->coord (last-move (current-game-state)))))

(defun make-human-move! (state s)
  "If given string represents chess move, apply it to current game."
  (when (and (move-string-p s)
             (allowedp state (coord->move s)))
    (alexandria:when-let ((new-state (apply-move state (coord->move s))))
      (add-game-state! new-state)
      t)))

(defun user-move (state s)
  "Helper function to handle user and ai moves.
   State arg included to avoid user moves when game is not set."
  (if (null (make-human-move! state s))
      (concatenate 'string "Illegal move: " s)
      (if (game-end-p (current-game-state))
          (result (current-game-state))
          (when (get-option :ai-mode)
            (let ((move (make-ai-move! (current-game-state))))
              (if (game-end-p (current-game-state))
                  (format nil "~a~%~a" move (result (current-game-state)))
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
      (cl-utilities:split-sequence #\= (string option))
    (set-option! (intern key :keyword) (if value value t))))

(defun unsupported-command (cmd)
  "Utility to return error message for unsupported commands."
  (concatenate 'string "Error (Unsupported command): " cmd))

(defun set-variant (variant)
  (if (eq variant "normal")
      (set-option! :variant "normal")
      (concatenate 'string "Error (unsupported variant given): " variant)))

(defun process-cecp-command (cmd)
  "Processes command in cecp mode."
  (cond ((ppcre:register-groups-bind ((#'parse-integer arg))
             ("^protover\\s(\\d)$" cmd)
           (progn (set-option! :cecp-protocol-version arg)
                  (list-cecp-supported-features))))
        ((ppcre:scan "^accepted$" cmd) nil)
        ((ppcre:scan "^rejected$" cmd) nil)
        ((ppcre:scan "^random$" cmd) nil)
        ((ppcre:scan "^new$" cmd) (progn (set-game! "startpos")
                                         (set-option! :ai-mode t)))
        ((ppcre:register-groups-bind ((#'parse-integer arg))
             ("^variant\\s(\\w?)$" cmd)
           (set-variant arg)))
        ((ppcre:scan "^force$" cmd) (set-option! :ai-mode nil))
        ((ppcre:scan "^go$" cmd) (set-option! :ai-mode t))
        ((ppcre:scan "^playother$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^level\\s.+" cmd) (unsupported-command cmd))
        ((ppcre:scan "^st\\s.+" cmd) (unsupported-command cmd))
        ((ppcre:register-groups-bind ((#'parse-integer arg))
             ("^sd\\s(\\d+)$"cmd)
           (set-option! :depth-limit arg)))
        ((ppcre:scan "^nps\\s" cmd) (unsupported-command cmd))
        ((ppcre:scan "^time\\s" cmd) (unsupported-command cmd))
        ((ppcre:scan "^otim\\s" cmd) (unsupported-command cmd))
        ((ppcre:register-groups-bind (arg)
             ("^usermove\\s(.+)$" cmd)
           (tursas-cmd "Can't make move in a empty board!" #'user-move arg)))
        ((ppcre:scan "^?$" cmd) (unsupported-command cmd))
        ((ppcre:register-groups-bind ((#'parse-integer arg))
             ("^ping\\s(\\d+)$" cmd)
           (concatenate 'string "pong " arg)))
        ((ppcre:scan "^draw$" cmd) (tursas-cmd "Can't offer draw to empty board!" #'cecp-draw))
        ((ppcre:scan "^result$" cmd) nil)
        ((ppcre:register-groups-bind (arg)
             ("^setboard\\s(.+)$" cmd)
           (set-game! arg)))
        ((ppcre:scan "^hint$" cmd) (tursas-cmd "Can't print hint from a empty board!" #'get-hint))
        ((ppcre:scan "^bk$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^undo$" cmd) (undo-move!))
        ((ppcre:scan "^remove$" cmd) (undo-move! 2))
        ((ppcre:scan "^hard$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^easy$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^post$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^nopost$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^analyse$" cmd) (unsupported-command cmd))
        ((ppcre:register-groups-bind (arg)
             ("^name\\s(.+)$" cmd)
           (set-option! :opponent-name arg)))
        ((ppcre:scan "^rating$" cmd) 100)
        ((ppcre:scan "^ics$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^computer$" cmd) (set-option! :opponent :cpu))
        ((ppcre:scan "^pause$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^resume$" cmd) (unsupported-command cmd))
        ((ppcre:scan "^memory\\s.+" cmd) (unsupported-command cmd))
        ((ppcre:scan "^cores\\s" cmd) (unsupported-command cmd))
        ((ppcre:scan "^egtpath\\s" cmd) (unsupported-command cmd))
        ((ppcre:register-groups-bind (args)
             ("^option\\s(.+)$" cmd)
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
  (cond ((ppcre:scan "^$" cmd) nil)
        ((ppcre:scan "^help$" cmd) (usage))
        ((ppcre:scan "^load$" cmd) (load-game))
        ((ppcre:scan "^save$" cmd) (save-game))
        ((ppcre:scan "^bd$" cmd) (tursas-cmd "Can't print empty board!" #'display-board))
        ((ppcre:scan "^fd$" cmd) (tursas-cmd "Can't display FEN for empty state." #'display-fen))
        ((ppcre:scan "^lm$" cmd) (tursas-cmd "Can't list moves from empty state." #'list-moves))
        ((ppcre:scan "^gs$" cmd) (tursas-cmd "Can't calculate score from empty state." #'get-score))
        ((ppcre:scan "^cp$" cmd) (progn (tursas-cmd "Can't make AI move on empty board!" #'make-ai-move!)
                                        (tursas-cmd "Can't print empty board!" #'display-board)))
        ((ppcre:scan "^es$" cmd) (tursas-cmd "Can't eval empty game state!" #'eval-current-state))
        ((ppcre:register-groups-bind ((#'parse-integer arg))
             ("^pf\\s(\\d+)$" cmd)
           (tursas-cmd "Can't calculate perft from empty game-state!"
                       #'display-perft arg)))
        ((ppcre:scan "^xboard$" cmd) (set-protocol! :cecp))
        ((ppcre:scan "^quit$" cmd) (quit))
        (t (if (eq (get-protocol) :cecp)
               (process-cecp-command cmd)
               (concatenate 'string "Error (Invalid command): " cmd)))))
