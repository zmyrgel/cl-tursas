(defpackage :tursas
  (:documentation "Main package of Tursas.")
  (:use :cl :alexandria :cl-ppcre)
  (:shadowing-import-from :tursas.state0x88
                          ;; Move0x88
                          "COORD->MOVE" "MOVE->COORD" "FROM" "TO" "PROMOTION"
                          ;; State0x88
                          "ALLOWEDP" "OCCUPIEDP" "BLACKP" "WHITEP" "CHECKP" "MATEP" "DRAWP"
                          "STATE->FEN" "RESULT" "LEGAL-STATES" "LEGAL-MOVES" "APPLY-MOVE"
                          "TURN" "PERFT" "DYNAMICP" "FULL-MOVES" "EVALUATE" "LAST-MOVE" "GAME-SCORE" "GAME-END-P"
                          "FEN->STATE")
  (:export main

   ;; ;; util
   ;; valid-coord-p split-move coordinate-string-p
   ;; san-string-p move-string-p fen->ascii
   ;; split-on expand-digits compact-item

   ;; ;; search
   ;; alpha-beta

   ;; ;; engine
   ;; process-command

   ))
