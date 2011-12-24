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

;; (defun tweak-text (lst caps lit)
;;   "Utility function to convert list of symbols to a proper text string."
;;   (when lst
;;     (let ((item (car lst))
;;           (rest (cdr lst)))
;;       (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
;;             ((member item '(#\! #\? #\. #\:)) (cons item (tweak-text rest t lit)))
;;             ((eql item #\") (tweak-text rest caps (not lit)))
;;             (lit (cons item (tweak-text rest nil lit)))
;;             (caps (cons (char-upcase item) (tweak-text rest nil lit)))
;;             (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;; (defun game-print (lst)
;;   "Prints the given sexp as string."
;;   (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
;;   (fresh-line))


;; (defun game-repl ()
;;   "The main game repl function."
;;   (let ((cmd (game-read)))
;;     (unless (eq (car cmd) 'quit)
;;       (game-print (game-eval cmd))
;;       (game-repl))))

;; (defun game-read ()
;;   "Game reader function"
;;   (let ((cmd (read-from-string
;;               (concatenate 'string "(" (read-line) ")"))))
;;     (flet ((quote-it (x)
;;              (list 'quote x)))
;;       (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; (defun game-eval (sexp)
;;   "Evaluate repl sexp."
;;   (cond ((valid-commandp sexp) (eval sexp))
;;         ((unimplemented-commandp sexp) '(command is not implemented by engine))
;;         (t '(i do not know that command.))))

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
