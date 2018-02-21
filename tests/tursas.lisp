(in-package :tursas.tests)

(def-suite tursas-suite :description "Tursas package test suite.")
(in-suite tursas-suite)

;; (test repl-test
;;   "Test game repl functionality."
;;   (is (with-output-to-string (s)
;;         (game-print (list "foo" "bar")))
;;       "foo\nbar")
;;   (is (with-output-to-string (s)
;;         (game-print "foo"))
;;       "foo")
;;   (is (with-output-to-string (s)
;;         (game-print 200))
;;       "200")
;;   (is-false (with-output-to-string (s)
;;               (game-print nil))))
