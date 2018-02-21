;;;; search provides the functions for doing alpha-beta search of the
;;;; chess game.
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
