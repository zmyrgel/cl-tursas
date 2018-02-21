(defpackage :tursas.utils
  (:documentation "Utilities used by Tursas.")
  (:use :cl)
  (:import-from :alexandria :define-constant :when-let)
  (:import-from :cl-utilities :split-sequence)
  (:export :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :string-indexed))
