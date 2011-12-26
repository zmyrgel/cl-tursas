(defpackage :tursas
  (:documentation "Main package of Tursas.")
  (:use :cl :alexandria :cl-ppcre :tursas.utils)
  (:shadowing-import-from :tursas.state0x88
                          ;; Move0x88
                          "COORD->MOVE" "MOVE->COORD" "FROM" "TO" "PROMOTION"
                          ;; State0x88
                          "ALLOWEDP" "OCCUPIEDP" "BLACKP" "WHITEP" "CHECKP" "MATEP" "DRAWP"
                          "STATE->FEN" "RESULT" "LEGAL-STATES" "LEGAL-MOVES" "APPLY-MOVE"
                          "TURN" "PERFT" "DYNAMICP" "FULL-MOVES" "EVALUATE" "LAST-MOVE" "GAME-SCORE" "GAME-END-P"
                          "FEN->STATE")
  (:export main))
