(defpackage :tursas.state0x88
  (:documentation "Package provides 0x88-based implementation.")
  (:use :cl :alexandria)
  (:export
   ;; move0x88
   coord->move move->coord from to promotion

   ;; state0x88
   allowedp occupiedp blackp whitep checkp matep drawp
   state->fen result legal-states legal-moves apply-move
   turn perft dynamicp full-moves evaluate last-move game-score game-end-p
   fen->state))
