;; Copyright (c) 2011, Timo Myyrä <timo.myyra@gmail.com>

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
        ((numberp output) (format t "~a~%" output))
        (t nil)))

(defun main (&rest args)
  "Starts the engine repl for input handling."
  (init-engine)
  (loop (game-print (game-eval (game-read)))))
