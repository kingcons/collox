(defpackage collox/tests/main
  (:use :cl
        :collox
        :rove)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :collox.scanner
                :tokenize))
(in-package :collox/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :collox)' in your Lisp.

(deftest test-tokenize-1
  (testing "should parse comments and basic operators"
    (ok (tokenize (read-file-into-string "scanner1.lox")))))

;; (deftest test-tokenize-2
;;   (testing "should parse a basic variable definition"
;;     (ok (tokenize "var language = \"lox\""))))
