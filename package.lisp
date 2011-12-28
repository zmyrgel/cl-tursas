;; Copyright (c) 2011, Timo Myyrä <timo.myyra@gmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :tursas
  (:documentation "Main package of Tursas.")
  (:use :cl)
  (:import-from :tursas.utils
   :valid-coord-p :split-move :coordinate-string-p
   :san-string-p :move-string-p :fen->ascii
   :split-on :expand-digits :compact-item :str :string-indexed)
  (:import-from :cl-ppcre
   :scan :register-groups-bind)
  (:import-from :alexandria
   :define-constant :iota :curry :if-let :when-let :alist-hash-table)
  (:import-from :cl-utilities
   :split-sequence)
  (:import-from :tursas.state0x88
   :coord->move :move->coord :from :to :promotion
   :allowedp :occupiedp :blackp :whitep :checkp :matep :drawp
   :state->fen :result :legal-states :legal-moves :apply-move
   :turn :perft :dynamicp :full-moves :evaluate :last-move
   :game-score :game-end-p :fen->state)
  (:export :main))
