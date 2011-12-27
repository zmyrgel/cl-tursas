(in-package :tursas)

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
          (t (alexandria:if-let ((children (legal-states state)))
               (f children (first children) alpha (1- depth))
               (f nil nil (funcall eval-fn state) 0))))))
