(defpackage cl-slack/tests/main
  (:use :cl
        :cl-slack
        :rove))
(in-package :cl-slack/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-slack)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
