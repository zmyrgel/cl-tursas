(defpackage :tursas
  (:documentation "Main package of Tursas.")
  (:use :cl)
  (:import-from :tursas.utils
   :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :string-indexed)
  (:import-from :cl-ppcre
   :scan :register-groups-bind)
  (:import-from :alexandria
   :define-constant :if-let :when-let)
  (:import-from :cl-utilities
   :split-sequence)
  (:import-from :tursas.state0x88
   :coord->move :move->coord :from :to :promotion
   :allowedp :occupiedp :blackp :whitep :checkp :matep :drawp
   :state->fen :result :legal-states :legal-moves :apply-move
   :turn :perft :dynamicp :full-moves :evaluate :last-move
   :game-score :game-end-p :fen->state)
  (:export :main))
