;; Copyright (c) 2012 Timo Myyrä <timo.myyra@iki.fi>

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

(defpackage :tursas-asd (:use :cl :asdf))

(in-package :tursas-asd)

(defsystem "tursas"
  :name "tursas"
  :version "0.2"
  :author "Timo Myyrä <timo.myyra@iki.fi>"
  :maintainer "Timo Myyrä <timo.myyra@iki.fi>"
  :description "Tursas"
  :long-description "a simple chess engine which uses XBoard protocol."
  :license "ISC license"
  :depends-on (:cl-ppcre :alexandria :cl-utilities :prove)
  :serial t
  :components ((:file "utils")
               (:module "state0x88"
                :serial t
                :components ((:file "package")
                             (:file "common0x88")
                             (:file "eval0x88")
                             (:file "move0x88")
                             (:file "board0x88")
                             (:file "movegen0x88")
                             (:file "fen0x88")
                             (:file "state0x88")))
               (:file "core")
               ;; (:module "t"
               ;;  :serial t
               ;;  :components ((:file "utils")
               ;;               (:file "state0x88")))
               ))
