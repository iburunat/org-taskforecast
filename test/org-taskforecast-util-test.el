;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-taskforecast-util)

(defmacro should-not-error (form) form)

;;; Test:

;;;; Debug

(ert-deftest org-taskforecast-assert ()
  (let ((org-taskforecast-enable-assert nil))
    (should-not-error (org-taskforecast-assert t))
    (should-not-error (org-taskforecast-assert nil)))
  (let ((org-taskforecast-enable-assert t))
    (should-not-error (org-taskforecast-assert t))
    (should-error (org-taskforecast-assert nil))))


;;;; Alist utility

(org-taskforecast-defalist defalist-test a b c)
(ert-deftest org-taskforecast-defalist ()
  ;; constructor
  (should (eq nil (alist-get 'a (defalist-test))))
  (should (= 1 (alist-get 'b (defalist-test :a 0 :b 1 :c 2))))
  ;; type checker
  (should (defalist-test-type-p (defalist-test)))
  (should (defalist-test-type-p'((c) (a) (b))))
  (should-not (defalist-test-type-p'((a) (b)))))

;;; org-taskforecast-util-test.el ends here
