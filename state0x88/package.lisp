(defpackage :tursas.state0x88
  (:use :cl :alexandria)
  (:export
   ;; move
   move->coord from to promotion

   ;; state
   allowedp occupiedp blackp whitep checkp matep drawp
   state->fen result legal-states legal-moves apply-move
   turn perft dynamicp full-moves evaluate last-move game-score game-end-p
   fen->state))
