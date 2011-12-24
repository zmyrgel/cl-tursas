(in-package :tursas)

(defun alpha-beta (alpha beta depth eval-fn state)
  "Find the best state, from initial state by searching to given depth
   and backing up values using cutoff whenever possible.
   Based on alpha-beta presented in PAIP"
  (labels ((f (states best-state ac depth)
             (if (null states)
                 (list ac best-state)
                 (destructuring-bind (val . discard)
                     (alpha-beta (- beta) (- ac) depth eval-fn (first states))
                   (let ((value (- val)))
                     (cond ((>= ac beta) (list ac best-state))
                           ((> value ac) (f (rest states) (first states) value depth))
                           (t (f (rest states) best-state ac depth))))))))
    (cond ((game-end-p state) (list (game-score state) nil))
          ((zerop depth) (list (funcall eval-fn state) nil))
          (t (alexandria:if-let ((children (legal-states state)))
               (f children (first children) alpha (1- depth))
               (f nil nil (funcall eval-fn state) 0))))))
