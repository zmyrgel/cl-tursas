;; Copyright (c) 2012, Timo Myyrä <timo.myyra@gmail.com>

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

(in-package :tursas.tests)

(def-suite state0x88-suite :description "State0x88 package test suite.")
(in-suite state0x88-suite)

;; (def-fixture person-schema []
;;   (with-connection hdb
;;     (try
;;      (do-commands "create table person (name varchar(255), age integer)")
;;      (insert-values :person [:name] ["bill"] ["joey"])
;;      (with-query-results results [query]
;;        (test-body))
;;      (finally
;;       (do-commands "drop schema public cascade")))))

;; (deftest test-ideal
;;   (with-fixture person-schema []
;;     (is (= "bill" (:name (first results))))
;;     (is (= "joey" (:name (second results)))))))

(test dummy
  (is (= 1 1)))
