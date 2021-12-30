(fiasco:define-test-package :collox/tests
  (:use :collox)
  (:import-from :alexandria :read-file-into-string)
  (:import-from :collox.scanner :tokenize :token-value))

(in-package :collox/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :collox)' in your Lisp.

(defun asset-path (namestring)
  (asdf:system-relative-pathname :collox namestring))

(deftest tokenize-operators ()
  (let ((source-path (asset-path "tests/scanner1.lox")))
    (finishes (tokenize (read-file-into-string source-path)))))

(deftest tokenize-strings ()
  (finishes (tokenize "()() \"foobar\""))
  (signals error (tokenize "()()\"unterminated")))

(deftest tokenize-numbers ()
  (finishes (tokenize "42"))
  (finishes (tokenize "123.589"))
  (signals error (tokenize "42.."))
  (signals error (tokenize "123.456.789"))
  (finishes (= (length (tokenize "42.4")) 1))
  (finishes (= 123.4 (token-value (car (tokenize "123.4"))))))

(deftest tokenize-variable ()
  (finishes (tokenize "var language = \"lox\"")))
