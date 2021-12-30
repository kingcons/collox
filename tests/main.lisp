(fiasco:define-test-package :collox.tests
  (:use :collox :collox.tests)
  (:import-from :alexandria :read-file-into-string)
  (:import-from :collox.scanner :tokenize :token-value :token-type :syntax-error))

(in-package :collox.tests)

;; NOTE: To run this test file, execute `(asdf:test-system :collox)' in your Lisp.

(defun asset-path (namestring)
  (asdf:system-relative-pathname :collox namestring))

(deftest tokenize-operators ()
  (let ((source-path (asset-path "tests/scanner1.lox")))
    (finishes (tokenize (read-file-into-string source-path)))))

(deftest tokenize-strings ()
  (finishes (tokenize "()() \"foobar\""))
  (signals syntax-error (tokenize "()()\"unterminated")))

(deftest tokenize-numbers ()
  (finishes (tokenize "42"))
  (finishes (tokenize "123.589"))
  (finishes (tokenize "1 + (2 + 3)"))
  (signals syntax-error (tokenize "42.."))
  (signals syntax-error (tokenize "123.456.789"))
  (is (= (length (tokenize "42.4")) 1))
  (is (= 7 (token-value (car (tokenize "7")))))
  (is (= 123.4 (token-value (car (tokenize "123.4"))))))

(deftest tokenize-variable ()
  (is (equalp (mapcar #'token-type (tokenize "var example = \"lox\""))
              '(:var :identifier :equal :string))))

(deftest tokenize-definition ()
  (is (equalp (mapcar #'token-value (tokenize "var example = 1 + (2+3);"))
              '("var" "example" nil 1 nil nil 2 nil 3 nil nil))))
