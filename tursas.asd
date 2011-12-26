(defpackage :tursas-asd (:use :cl :asdf))

(in-package :tursas-asd)

(defsystem "tursas"
  :name "tursas"
  :version "0.0.1"
  :author "Timo Myyrä <timo.myyra@gmail.com>"
  :maintainer "Timo Myyrä <timo.myyra@gmail.com>"
  :description "Tursas"
  :long-description "a simple chess engine which uses XBoard protocol."
  :licence "ISC license"
  :depends-on (:cl-ppcre :alexandria :cl-utilities)
  :serial t
  :components ((:module "state0x88"
                :serial t
                :components ((:file "package")
                             (:file "common0x88")
                             (:file "eval0x88")
                             (:file "move0x88")
                             (:file "board0x88")
                             (:file "movegen0x88")
                             (:file "fen0x88")
                             (:file "state0x88")))
               (:file "package")
               (:file "util")
               (:file "search")
               (:file "engine" :depends-on ("state0x88"))
               (:file "repl")))
