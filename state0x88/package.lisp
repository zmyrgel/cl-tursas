(defpackage :tursas.state0x88
  (:documentation "Package provides 0x88-based implementation.")
  (:use :cl)
  (:import-from :cl-utilities :split-sequence)
  (:import-from :alexandria
   :define-constant :when-let :if-let :curry :iota :hash-table-keys :hash-table-values :copy-array :copy-hash-table :hash-table-alist)
  (:import-from :tursas.utils
   :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :str :string-indexed)
  (:export
   :coord->move :move->coord :from :to :promotion
   :allowedp :occupiedp :blackp :whitep :checkp :matep :drawp
   :state->fen :result :legal-states :legal-moves :apply-move
   :turn :perft :dynamicp :full-moves :evaluate :last-move :game-score :game-end-p
   :fen->state))
