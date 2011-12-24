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
  :components ((:module "state0x88"
                :components ((:file "package")
                             (:file "common0x88" :depends-on ("package"))
                             (:file "eval0x88" :depends-on ("package" "common0x88"))
                             (:file "move0x88" :depends-on ("package" "common0x88"))
                             (:file "movegen0x88" :depends-on ("package" "common0x88"))
                             (:file "board0x88" :depends-on ("package" "common0x88" "eval0x88"))
                             (:file "fen0x88" :depends-on ("package" "common0x88"))
                             (:file "state0x88" :depends-on ("package" "common0x88" "board0x88" "move0x88" "fen0x88" "eval0x88"))))
               (:file "package")
               (:file "util" :depends-on ("package"))
               (:file "state" :depends-on ("package"))
               (:file "move" :depends-on ("package"))
               (:file "search" :depends-on ("package"))
               (:file "engine" :depends-on ("package" "state0x88" "util" "search" "state" "move"))
               (:file "repl" :depends-on ("package" "engine"))))
