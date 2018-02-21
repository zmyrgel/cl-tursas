(defpackage :tursas.tests
  (:documentation "Test package of Tursas.")
  (:use :cl :fiveam)
  (:import-from :tursas.utils
   :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :string-indexed)
  (:import-from :tursas.state0x88
   :coord->move :move->coord :from :to :promotion
   :allowedp :occupiedp :blackp :whitep :checkp :matep :drawp
   :state->fen :result :legal-states :legal-moves :apply-move
   :turn :perft :dynamicp :full-moves :evaluate :last-move
   :game-score :game-end-p :fen->state)
  (:export :run-tests))
