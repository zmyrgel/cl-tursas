(defpackage :tursas-asd (:use :cl :asdf))

(in-package :tursas-asd)

(defsystem "tursas"
  :name "tursas"
  :version "0.2"
  :author "Timo Myyrä <timo.myyra@iki.fi>"
  :maintainer "Timo Myyrä <timo.myyra@iki.fi>"
  :description "Tursas - a simple chess engine which uses XBoard protocol."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :license "ISC license"
  :depends-on ("cl-ppcre" "alexandria" "str" "unix-opts")
  :serial t
  :components ((:file "utils")
               (:module "state0x88"
                :serial t
                :components ((:file "package")
                             (:file "common")
                             (:file "eval")
                             (:file "move")
                             (:file "board")
                             (:file "movegen")
                             (:file "fen")
                             (:file "core")))
               (:file "core"))
  :in-order-to ((test-op (test-op "tursas/tests"))))

(defsystem "tursas/tests"
  :name "tursas/tests"
  :version "0.2"
  :author "Timo Myyrä <timo.myyra@iki.fi>"
  :maintainer "Timo Myyrä <timo.myyra@iki.fi>"
  :description "Tests for the Tursas chess engine"
  :license "ISC license"
  :depends-on ("tursas" "rove")
  :serial t
  :components ((:module "t"
                :components ((:file "utils")
                             (:file "state0x88"))))
  :perform (test-op (op c)
                    (funcall (read-from-string "rove:run")
                             :tursas/tests :style :spec)))
