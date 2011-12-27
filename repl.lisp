(in-package :tursas)

(defconstant +implemented-cecp-commands+
  '(protover accepted rejected new
    variant force go sd usermove ping
    draw result setboard hint undo remove
    name rating computer option))

(defconstant +implemented-general-commands+
  '(help load save bd fd lm gs es pf xboard quit))

;; XXX: not used yet
(defconstant +unimplemented-xboard-commands+
  '(playother level st nps time otim ?
    bk hard easy post nopost analyse
    ics pause resume memory cores egtpath))

(defmacro valid-commandp (sexp)
  "Checks if given command can be run."
  `(or (member ,(car sexp) +implemented-cecp-commands+)
       (member ,(car sexp) +implemented-general-commands+)))

(defmacro unimplemented-commandp (sexp)
  "Checks if given command is known but not implemented."
  `(member ,(car sexp) +unimplemented-cecp-commands+))

(defun init-engine ()
  "Initializes the chess engine."
  (format t "~{~a~%~}"
          '("# Welcome to Tursas Chess Engine!"
            "# Type 'help' to get list of supported commands")))

(defun game-eval (s)
   "Evaluates given engine protocol command."
  (process-command s))

(defun game-read ()
  "Reader function to parse commandline.
   Reads user input as a string and converts it to sexp."
  (read-line))

(defun game-print (output)
  "Prints prompt and responses to user."
  (cond ((listp output) (format t "~{~a~%~}" output))
        ((stringp output) (format t "~a~%" output))
        (t nil)))

(defun main (&rest args)
  "Starts the engine repl for input handling."
  (init-engine)
  (loop (game-print (game-eval (game-read)))))
