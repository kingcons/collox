(defpackage collox/tests/main
  (:use :cl
        :collox
        :rove))
(in-package :collox/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :collox)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
