;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-taskforecast)

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

;;;; Cache

(ert-deftest org-taskforecast--memoize-cache-table-op ()
  (org-taskforecast--memoize-use-cache
      (org-taskforecast--memoize-make-cache-table)
    (should (null (org-taskforecast--memoize-get "id" 'key)))
    (should-not (org-taskforecast--memoize-exists-p "id" 'key))

    (org-taskforecast--memoize-set "id" 'key 123)
    (should (org-taskforecast--memoize-exists-p "id" 'key))
    (should (= 123 (org-taskforecast--memoize-get "id" 'key)))

    (org-taskforecast--memoize-drop "id")
    (should (null (org-taskforecast--memoize-get "id" 'key)))
    (should-not (org-taskforecast--memoize-exists-p "id" 'key))))

(ert-deftest org-taskforecast--memoize ()
  (org-taskforecast--memoize-use-cache
      (org-taskforecast--memoize-make-cache-table)
    (let* ((counter 0)
           (fn (lambda ()
                 (org-taskforecast--memoize "id"
                   (cl-incf counter)))))
      (should (= 1 (funcall fn)))
      (should (= 1 (funcall fn)))
      (org-taskforecast--memoize-use-cache
          (org-taskforecast--memoize-make-cache-table)
        ;; cache not found and run body of fn
        (should (= 2 (funcall fn)))
        (should (= 2 (funcall fn))))
      (should (= 1 (funcall fn))))))

;;; org-taskforecast-test.el ends here

;; Local Variables:
;; byte-compile-error-on-warn: t
;; End:
