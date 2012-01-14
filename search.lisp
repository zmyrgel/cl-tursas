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

(declaim (optimize (speed 3)
                   (safety 0)))

(defun alpha-beta (alpha beta depth eval-fn state)
  "Find the best state, from initial state by searching to given depth
   and backing up values using cutoff whenever possible.
   Based on alpha-beta presented in PAIP"
  (declare (fixnum alpha beta depth))
  (labels ((f (states best-state ac depth)
             (declare (fixnum ac depth))
             (if (null states)
                 (values ac best-state)
                 (let ((value (the fixnum (- (the fixnum (alpha-beta (the fixnum (- beta))
                                                                     (the fixnum (- ac))
                                                                     depth
                                                                     eval-fn
                                                                     (first states)))))))
                   (cond ((>= ac beta) (values ac best-state))
                         ((> value ac) (f (rest states) (first states) value depth))
                         (t (f (rest states) best-state ac depth)))))))
    (cond ((game-end-p state) (values (game-score state) nil))
          ((zerop depth) (values (funcall (the function eval-fn) state) nil))
          (t (if-let ((children (legal-states state)))
               (f children (first children) alpha (1- depth))
               (f nil nil (funcall (the function eval-fn) state) 0))))))
