;;;; global tursas engine definitions
(in-package :cl-user)
(defpackage tursas.core
  (:documentation "Core of Tursas chess engine.")
  (:use :cl)
  (:import-from :tursas.utils
   :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :string-indexed)
  (:import-from :cl-ppcre
   :scan :register-groups-bind)
  (:import-from :alexandria
   :define-constant :if-let :when-let)
  (:import-from :tursas.state0x88
   :coord->move :move->coord :from :to :promotion
   :allowedp :occupiedp :blackp :whitep :checkp :matep :drawp
   :state->fen :result :legal-states :legal-moves :apply-move
   :turn :perft :dynamicp :full-moves :evaluate :last-move
   :game-score :game-end-p :fen->state)
  (:export :main))
(in-package :tursas.core)

;; body

;;; protocols lists generic functions available when working with
;;; different chess board representations.

;; Move protocol

(defgeneric move->coord (move)
  (:method (move) (error "Can't use generic move->coord!"))
  (:documentation "Takes a move and returns its coordinate representation."))

(defgeneric from (move)
  (:method (move) (error "Can't use generic from!"))
  (:documentation "Returns moves originating squares coordinate."))

(defgeneric to (move)
  (:method (move) (error "Can't use generic to!"))
  (:documentation "Returns moves destination squares coordinate."))

(defgeneric promotion (move)
  (:method (move) (error "Can't use generic promotion!"))
  (:documentation "Returns possible promotion character from a move"))

;; State protocol

(defgeneric allowedp (state move)
  (:method (state move) (error "Can't use generic function!"))
  (:documentation "Predicate to see if move is allowed in given state."))

(defgeneric occupiedp (state index)
  (:method (state index) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given index is occupied in the state."))

(defgeneric blackp (state index)
  (:method (state index) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given index is occupied by black player in the state."))

(defgeneric whitep (state index)
  (:method (state index) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given index is occupied by white player in the state."))

(defgeneric checkp (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given state is in check."))

(defgeneric matep (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given state is in mate."))

(defgeneric drawp (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given state is in draw."))

(defgeneric state->fen (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the FEN representation of given state."))

(defgeneric result (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the game result from the given state."))

(defgeneric legal-states (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns all the legal states from given state."))

(defgeneric legal-moves (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns all the legal moves from given state."))

(defgeneric apply-move (state move)
  (:method (state move) (error "Can't use generic function!"))
  (:documentation "Returns new state with with given move applied to state."))

(defgeneric turn (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns which players turn it is."))

(defgeneric perft (state depth)
  (:method (state depth) (error "Can't use generic function!"))
  (:documentation "Calculates perft score to given depth from state."))

(defgeneric dynamicp (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if state is 'dynamic'."))

(defgeneric full-moves (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns count of full moves the game has lasted."))

(defgeneric evaluate (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns heuristic score for given state."))

(defgeneric last-move (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the last move which lead to state."))

(defgeneric game-score (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the game's end score."))

(defgeneric game-end-p (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if game has ended"))

(define-constant +cecp-supported-features+
    '((ping . 1)
      (setboard . 1)
      (playother . 1)
      (san . 0)
      (usermove . 0)
      (time . 0)
      (draw . 1)
      (sigint . 0)
      (sigterm . 0)
      (reuse . 0)
      (analyze . 0)
      (myname . "\"Tursas 0.3\"")
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
  :test 'equal
  :documentation "alist of supported features in tursas chess engine.")

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
      "playother - Tell engine to switch sides"
      "level MPS BASE INC - set time controls"
      ;;"st TIME - set time controls"
      "sd DEPTH - set search depth to DEPTH"
      ;;"nps NODE_RATE - search only NODE_RATE nodes"
      "time N - set the engine clock to N centiseconds"
      "otim N - set the opponents clock"
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
      ;;"analyze - tell engine to engage analyze mode"
      "name X - tell engine its opponents name"
      "rating - ask engine its rating"
      ;;"ics - tell engine its engaging in ICS game"
      "computer - tell engine that its playing against cpu"
      ;;"pause - pause all actions"
      ;;"resume - resume all paused actions"
      ;;"memory N - specify how much engine can use memory"
      ;;"cores N - tell engine how many cpu cores it can use"
      ;;"egtpath PATH - tell engine to use end-game tables from PATH"
      "option NAME[=VALUE] - tell engine to set the engine feature NAME to VALUE")
  :test 'equal)

(defvar *debug-output* nil)
(defvar *engine-output* nil)
(defvar *engine-input* nil)

(defparameter *protocol* 'general)
(defparameter *game-state* nil)
(defparameter *game-options* '((:search-depth . 0)
                               (:ai-mode . nil)
                               (:cecp-protocol-version . 2)
                               (:debug . nil)
                               (:ponder . nil)
                               (:ponder-output . nil)
                               (:game-running . nil)
                               (:white-clock . 0)
                               (:black-clock . 0)
                               (:moves-per-time-control . nil)
                               (:seconds-per-time-control . nil)
                               (:clock-increment . nil)
                               (:white-player . nil)
                               (:black-player . nil)
                               (:options . nil)))

(defun current-game-state ()
  "Utility to return current game state or nil."
  (first *game-state*))

(defun set-option! (key value)
  "Sets game option of given key to value."
  (setf (rest (assoc key *game-options*)) value))

(defun get-option (option)
  "Returns value of given game option"
  (cdr (assoc option *game-options*)))

(defun alpha-beta (alpha beta depth eval-fn state)
  "Find the best state, from initial state by searching to given depth
   and backing up values using cutoff whenever possible.
   Based on alpha-beta presented in PAIP"
  (labels ((f (states best-state ac depth)
             (if (null states)
                 (values ac best-state)
                 (let ((value (- (alpha-beta (- beta) (- ac) depth eval-fn (first states)))))
                   (cond ((>= ac beta) (values ac best-state))
                         ((> value ac) (f (rest states) (first states) value depth))
                         (t (f (rest states) best-state ac depth)))))))
    (cond ((game-end-p state) (values (game-score state) nil))
          ((zerop depth) (values (funcall eval-fn state) nil))
          (t (if-let ((children (legal-states state)))
               (f children (first children) alpha (1- depth))
               (f nil nil (funcall eval-fn state) 0))))))

(defun save-game ()
  "Saves the current game by writing game-state to file."
  (with-open-file (s "saved-game.txt" :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (princ *game-state* s))
    nil))

(defun load-game ()
  "Loads the game-state from file to resume the previous game."
  (with-open-file (stream "saved-game.txt" :if-does-not-exist nil)
    (with-standard-io-syntax
      (if (null stream)
          "failed to load game, no file found!"
          (loop for form = (read stream nil 'eof)
                until (eq form 'eof)
                do (setf *game-state* form))))))

(defun get-protocol ()
  "Returns currently active reader protocol"
  *protocol*)

(defun set-protocol! (prot)
  "Sets the currently active reader protocol"
  (setf *protocol* prot))

(defun tursas-cmd (msg f &rest args)
  "Wrapper for commands which use current game state."
  (if (null (current-game-state))
      (str:concat "Error (" msg ")")
      (apply f (current-game-state) args)))

(defun add-game-state! (new-state)
  "Adds given state to game state."
  (push new-state *game-state*)
  t)

(defun display-board (state)
  "Displays the current chess board in ASCII."
  (str:concat
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
  (alpha-beta most-negative-fixnum
              most-positive-fixnum
              (get-option :search-depth)
              #'evaluate state))

(defun eval-current-state (state)
  "Evaluates the current state and returns its score."
  (str:concat (evaluate state)))

(defun get-hint (state)
  "Evaluates all states and chooses one from top five moves at random."
  (multiple-value-bind (score best-state)
      (alpha-beta most-negative-fixnum most-positive-fixnum 2 #'evaluate state)
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
      (str:concat "Error (Invalid setboard command): " s))))

(defun toggle-option! (option)
  "Toggles the value of given boolean game option."
  (set-option! option (not (get-option option))))

(defun current-player-type ()
  "Returns the type of player whose turn it is."
  (let ((current-player (turn (current-game-state))))
    (if (eq current-player :white)
        (get-option :white-player)
        (get-option :black-player))))

(defun make-ai-move! (state)
  "Function instructs chess engine to choose move for active player."
  (let ((move (choose-move state)))
    (process-command move)
    (str:concat "move " move)))

(defun choose-move (state)
  "Tell engine to choose an best state it can from current game state."
  (multiple-value-bind (score best-state)
      (alpha-beta most-negative-fixnum
                  most-positive-fixnum
                  (get-option :search-depth)
                  #'evaluate state)
    (declare (ignore score))
    (move->coord (last-move best-state))))

(defun make-chess-move! (state s)
  "Apply given chess move S to game STATE. If move is invalid return
nil, otherwise returns t."
  (when (and (user-move-p s)
             (allowedp state (coord->move s)))
    (when-let ((new-state (apply-move state (coord->move s))))
      (add-game-state! new-state))))

(defun game-result (state)
  "Returns string representation for game result."
  (case (result state)
    (:fifty-move-rule "1/2-1/2 {50-move rule}")
    (:fide-draw "1/2-1/2 {Draw per FIDE rules}")
    (:stalemate "1/2-1/2 {Stalemate}")
    (:repetition "1/2-1/2 {Draw by repetition}")
    (:mate-for-black "0-1 {Black mates}")
    (:mate-for-white "1-0 {White mates}")
    (otherwise nil)))

(defun apply-chess-move (state s)
  "Helper function to handle chess moves.
   State arg included to avoid user moves when game is not set."
  ;; TODO: stop the opponent's clock
  ;; start the engine's clock, start
  ;; thinking, and eventually make a move.
  (if (null (make-chess-move! state s))
      (str:concat "Illegal move: " s)
      (when (game-end-p (current-game-state))
        (set-option! :game-running nil)
        (game-result (current-game-state)))))

(defun undo-move! (&optional (n 1))
  "Undo last move or if N given, N last moves."
  (setf *game-state* (nthcdr n *game-state*)))

(defun list-cecp-supported-features ()
  "Prints the default features of the engine."
  (format nil "feature ~{~a~^ ~}"
          (loop for (key . value) in +cecp-supported-features+
                collect (format nil "~a=~a" (string-downcase key) value))))

;; TODO: actually implement logic to accept or reject draw offers
(defun cecp-draw (state)
  "Offer draw to opponent."
  (when (get-option :ai-mode)
    (str:concat "1/2-1/2 {" (if (eq (turn state) :white)
                                "WHITE"
                                "BLACK")
                " offered a draw!}")))

(defun cecp-accept-feature (feature)
  "Instructs the engine that given FEATURE is accepted."
  (declare (ignore feature))
  t)

(defun cecp-reject-feature (feature)
  "Instructs the engine that given FEATURE is rejected."
  (declare (ignore feature))
  t)

(defun cecp-parse-option (option)
  "Wrapper to parse options from string and set them."
  (destructuring-bind (key &optional value)
      (str:split #\= (string option))
    (set-option! (intern key :keyword) (if value value t))))

(defun unsupported-command (cmd)
  "Utility to return error message for unsupported commands."
  (str:concat "Error (Unsupported command): " cmd))

(defun set-variant (variant)
  (if (string= variant "normal")
      (set-option! :variant "normal")
      (str:concat "Error (unsupported variant given): " variant)))

(defun user-move-p (s)
  "Predicate to check if given string S represents user move."
  (scan "^[a-h]{1}[1-8]{1}[a-h]{1}[1-8]{1}[rnbq]?$" s))

(defun cecp-process-option (option)
  "Sets the given OPTION in the engine."
  (declare (ignore option))
  nil)

(defun process-cecp-command (cmd)
  "Processes command in cecp mode."
  (cond ((register-groups-bind ((#'parse-integer arg))
             ("^protover\\s(\\d)$" cmd)
           (set-option! :cecp-protocol-version arg)
           (list-cecp-supported-features)))
        ((register-groups-bind (feature)
             ("^accepted\\s(\\w+)$" cmd)
           (cecp-accept-feature feature)))
        ((register-groups-bind (feature)
             ("^rejected\\s(\\w+)$" cmd)
           (cecp-reject-feature feature)))
        ((string= "random" cmd)
         nil)
        ((string= "new" cmd)
         (set-game! "startpos")
         (set-option! :search-depth 10) ;; not unlimited but might as well be with this engine
         (set-option! :game-running t)
         (set-option! :ai-mode t)
         (set-option! :white-player :human)
         (set-option! :black-player :ai)
         (set-option! :white-clock 0)
         (set-option! :black-clock 0)
         ;; Associate the engine's clock with Black and the opponent's clock
         ;; with White. Reset clocks and time controls to the start of
         ;; a new game. Use wall clock for time measurement. Stop
         ;; clocks. Do not ponder on this move, even if pondering is
         ;; on. Remove any search depth limit previously set by the sd
         ;; command.
         t
         )
        ((register-groups-bind ((#'parse-integer arg))
             ("^variant\\s(\\w+)$" cmd)
           (set-variant arg)))
        ((string= "force" cmd)
         (set-option! :ai-mode nil)
         (set-option! :white-player :human)
         (set-option! :black-player :human)
         ;; TODO: stop clocks
         )
        ((string= "go" cmd)
         (set-option! :ai-mode t)
         (if (eq (turn (current-game-state)) :white)
             (progn
               (set-option! :white-player :ai)
               (set-option! :black-player :human))
             (progn
               (set-option! :white-player :human)
               (set-option! :black-player :ai)))
         ;; TODO: Associate the engine's clock with the color that is
         ;; on move, the opponent's clock with the color that is not
         ;; on move. Start the engine's clock.
         t)
        ((string= "playother" cmd)
         (rotatef (cdr (assoc :black-player *game-options*))
                  (cdr (assoc :white-player *game-options*)))
         t)
        ((register-groups-bind ((#'parse-integer mps) base (#'parse-integer inc))
             ("^level\\s(\\d+)\\s([0-9:]+)\\s(\\d+)$" cmd)
           (set-option! :moves-per-time-control mps)
           (destructuring-bind (min &optional (sec "0"))
               (str:split #\: base)
             (set-option! :seconds-per-time-control (+ (* 60 (parse-integer min)) (parse-integer sec))))
           (set-option! :clock-increment inc)
           t))
        ((register-groups-bind ((#'parse-integer arg))
             ("^st\\s(\\d+)$" cmd)
           (set-option! :search-time arg)
           t))
        ((register-groups-bind ((#'parse-integer arg))
             ("^sd\\s(\\d+)$" cmd)
           (set-option! :search-depth arg)
           t))
        ((scan "^nps\\s\\d+$" cmd)
         (unsupported-command cmd))
        ((register-groups-bind ((#'parse-integer arg))
             ("^time\\s(\\d+)$" cmd)
           (if (eq (turn (current-game-state)) :white)
               (set-option! :white-clock arg)
               (set-option! :black-clock arg))
           t))
        ((register-groups-bind ((#'parse-integer arg))
             ("^otim\\s(\\d+)$" cmd)
           (if (eq (turn (current-game-state)) :white)
               (set-option! :black-clock arg)
               (set-option! :white-clock arg))
           t))
        ((register-groups-bind (arg)
             ("^usermove\\s([a-h]{1}[1-8]{1}[a-h]{1}[1-8]{1}[rnbq]?)$" cmd)
           (tursas-cmd "Can't make move in a empty board!" #'apply-chess-move arg)))
        ((user-move-p cmd)
         (tursas-cmd "Can't make move in a empty board!" #'apply-chess-move cmd))
        ((string= "?" cmd) (unsupported-command cmd))
        ((register-groups-bind ((#'parse-integer arg))
             ("^ping\\s(\\d+)$" cmd)
           (str:concat "pong " (write-to-string arg))))
        ((string= "draw" cmd)
         (tursas-cmd "Can't offer draw to empty board!" #'cecp-draw))
        ((register-groups-bind (comment)
             ("^result\\s(.+)$" cmd)
           (declare (ignore comment))
           (set-option! :game-running nil)
           t))
        ((register-groups-bind (arg)
             ("^setboard\\s(.+)$" cmd)
           (set-game! arg)))
        ((string= "hint" cmd)
         (tursas-cmd "Can't print hint from a empty board!" #'get-hint))
        ((string= "bk" cmd)
         (unsupported-command cmd))
        ((string= "undo" cmd)
         (undo-move!))
        ((string= "remove" cmd)
         (undo-move! 2))
        ((string= "hard" cmd)
         (unsupported-command cmd))
        ((string= "easy" cmd)
         (unsupported-command cmd))
        ((string= "post" cmd)
         (unsupported-command cmd))
        ((string= "nopost" cmd)
         (unsupported-command cmd))
        ((string= "analyze" cmd)
         (unsupported-command cmd))
        ((register-groups-bind (arg)
             ("^name\\s(\\w+)$" cmd)
           (set-option! :opponent-name arg)))
        ((string= "rating" cmd)
         100)
        ((string= "ics" cmd)
         (unsupported-command cmd))
        ((string= "computer" cmd)
         (set-option! :opponent :cpu))
        ((string= "pause" cmd)
         (unsupported-command cmd))
        ((string= "resume" cmd)
         (unsupported-command cmd))
        ((scan "^memory\\s\\d+$" cmd)
         (unsupported-command cmd))
        ((scan "^cores\\s\\d+$" cmd)
         (unsupported-command cmd))
        ((scan "^egtpath\\s[\\w\\/]+$" cmd)
         (unsupported-command cmd))
        ((and (str:starts-with-p "option " cmd)
              (cecp-process-option (str:split #\= (subseq cmd 7)))))
        (t
         (str:concat "Error (Invalid command): " cmd))))

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
  ;(format t "Processing cmd: ~A~%" cmd)
  (cond ((string= "" cmd)
         nil)
        ((string= "help" cmd)
         (usage))
        ((string= "load" cmd)
         (load-game))
        ((string= "save" cmd)
         (save-game))
        ((string= "bd" cmd)
         (tursas-cmd "Can't print empty board!" #'display-board))
        ((string= "fd" cmd)
         (tursas-cmd "Can't display FEN for empty state." #'display-fen))
        ((string= "lm" cmd)
         (tursas-cmd "Can't list moves from empty state." #'list-moves))
        ((string= "gs" cmd)
         (tursas-cmd "Can't calculate score from empty state." #'get-score))
        ((string= "cp" cmd)
         (tursas-cmd "Can't make AI move on empty board!" #'make-ai-move!)
         (tursas-cmd "Can't print empty board!" #'display-board))
        ((string= "es" cmd)
         (tursas-cmd "Can't eval empty game state!" #'eval-current-state))
        ((register-groups-bind ((#'parse-integer arg))
             ("^pf\\s(\\d+)$" cmd)
           (tursas-cmd "Can't calculate perft from empty game-state!"
                       #'display-perft arg)))
        ((string= "xboard" cmd)
         (set-protocol! :cecp))
        ((eq (get-protocol) :cecp)
         (process-cecp-command cmd))
        (t
         (str:concat "Error (Invalid command): " cmd))))

(defun ai-turn-p ()
  "Predicate to check if its engines turn to make move."
  (and (get-option :game-running)
       (eq (current-player-type) :ai)))

(defun init-engine (options)
  "Function sets engine variables to correct values based on given OPTIONS."
  (if-let ((debug-filepath (getf options :debug)))
    (setf *debug-output* (open debug-filepath :direction :output :if-exists :supersede))
    (setf *debug-output* (make-broadcast-stream)))
  (setf *engine-output* (make-broadcast-stream
                         *debug-output*
                         (make-synonym-stream '*standard-output*)))
  (setf *engine-input* (make-synonym-stream '*standard-input*))

  (setf *protocol* 'general)
  (setf *game-state* nil)
  (setf *game-options* (copy-tree '((:search-depth . 0)
                                    (:ai-mode . nil)
                                    (:cecp-protocol-version . 2)
                                    (:debug . nil)
                                    (:ponder . nil)
                                    (:ponder-output . nil)
                                    (:game-running . nil)
                                    (:white-clock . 0)
                                    (:black-clock . 0)
                                    (:moves-per-time-control . nil)
                                    (:seconds-per-time-control . nil)
                                    (:clock-increment . nil)
                                    (:white-player . nil)
                                    (:black-player . nil)
                                    (:options . nil)))))

(defun run-engine ()
  "Run the chess engine."
  (loop for ai-turn = (ai-turn-p)
        for command = (read-line *engine-input*)
          then (if ai-turn
                   (make-ai-move! (current-game-state))
                   (read-line *engine-input*))
        until (string= command "quit")
        do (progn
             (format *debug-output* "INPUT: ~a~%" command)
             (let ((result (if ai-turn
                               command
                               (process-command command))))
               ;; skip printing on boolean values
               (typecase result
                 (list (format *engine-output* "~{~a~%~}" result))
                 (string (format *engine-output* "~a~%" result))
                 (number (format *engine-output* "~a~%" result)))))))

(defun main (&rest args)
  "Starts the engine repl for input handling."
  (declare (ignore args))
  (format t "~{~a~%~}"
          '("# Welcome to Tursas Chess Engine!"
            "# Type 'help' to get list of supported commands"))
  (opts:define-opts
      (:name :debug
       :description "output debug information to file FILE"
       :short #\d
       :long "debug"
       :arg-parser #'identity
       :meta-var "FILE"))
  (multiple-value-bind (options free-args)
      (opts:get-opts)
    (declare (ignore free-args))
    (init-engine options))
  (unwind-protect
       (handler-case
           (run-engine)
         (#+sbcl sb-sys:interactive-interrupt
          #+ccl  ccl:interrupt-signal-condition
          #+clisp system::simple-interrupt-condition
          #+ecl ext:interactive-interrupt
          #+allegro excl:interrupt-signal
          ()
           (run-engine)))
    (close *debug-output*)))
