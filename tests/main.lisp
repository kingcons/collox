(defpackage collox/tests/main
  (:use :cl
        :collox
        :rove)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :collox.scanner
                :tokenize
                :token-value))
(in-package :collox/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :collox)' in your Lisp.

(defun asset-path (namestring)
  (asdf:system-relative-pathname :collox namestring))

(deftest test-tokenize-1
  (testing "should parse comments and basic operators"
    (let ((source-path (asset-path "tests/scanner1.lox")))
      (ok (tokenize (read-file-into-string source-path))))))

(deftest test-tokenize-2
  (testing "should handle string literals"
    (ok (tokenize "()() \"foobar\""))
    (signals (tokenize "()()\"unterminated"))))

(deftest test-tokenize-3
  (testing "should handle numeric literals"
    (ok (tokenize "42"))
    (ok (tokenize "123.589"))
    (signals (tokenize "42.."))
    (signals (tokenize "123.456.789"))
    (ok (= (length (tokenize "42.4")) 1))
    (ok (= 123.4 (token-value (car (tokenize "123.4")))))))

;; (deftest test-tokenize-4
;;   (testing "should parse a basic variable definition"
;;     (ok (tokenize "var language = \"lox\""))))
