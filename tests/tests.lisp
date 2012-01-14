(in-package :tursas.tests)

;; Define test suites

(def-suite utils-suite :description "Utils package test suite.")
(def-suite tursas-suite :description "Tursas package test suite.")
(def-suite state0x88-suite :description "State0x88 package test suite.")

(defun run-tests ()
  (explain! (run 'utils-suite)))
