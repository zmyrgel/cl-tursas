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
