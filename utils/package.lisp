(defpackage :tursas.utils
  (:documentation "Utilities used by Tursas.")
  (:use :cl :alexandria :cl-ppcre)
  (:export "VALID-COORD-P" "SPLIT-MOVE" "COORDINATE-STRING-P"
           "SAN-STRING-P" "MOVE-STRING-P" "FEN->ASCII"
           "SPLIT-ON" "EXPAND-DIGITS" "COMPACT-ITEM" "STR" "STRING-INDEXED"))
