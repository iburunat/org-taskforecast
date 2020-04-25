;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-taskforecast)

(defmacro should-not-error (form) form)

;;; Test:

;;;; Cache

(defmacro with-cache-minor-mode (&rest body)
  "Evaluate BODY with clean cache."
  (declare (indent 0))
  `(progn
     ;; reset cache table
     (when org-taskforecast-cache-mode
       (org-taskforecast-cache-mode -1))
     (org-taskforecast-cache-mode +1)
     ,@body
     (org-taskforecast-cache-mode -1)))

(ert-deftest org-taskforecast--memoize-cache-table-op ()
  (with-cache-minor-mode
    (should (null (org-taskforecast--memoize-get "id" 'key)))
    (should-not (org-taskforecast--memoize-exists-p "id" 'key))

    (org-taskforecast--memoize-set "id" 'key 123)
    (should (org-taskforecast--memoize-exists-p "id" 'key))
    (should (= 123 (org-taskforecast--memoize-get "id" 'key)))

    (org-taskforecast--memoize-drop "id")
    (should (null (org-taskforecast--memoize-get "id" 'key)))
    (should-not (org-taskforecast--memoize-exists-p "id" 'key))))

(ert-deftest org-taskforecast--memoize ()
  (let* ((counter 0)
         (fn (lambda ()
               (org-taskforecast--memoize "id"
                 (cl-incf counter)))))
    (with-cache-minor-mode
      (should (= 1 (funcall fn)))
      (should (= 1 (funcall fn))))
    ;; cache not found and run body of fn
    (should (= 2 (funcall fn)))
    (with-cache-minor-mode
      (should (= 3 (funcall fn)))
      (should (= 3 (funcall fn))))
    (should (= 4 (funcall fn)))))

;;; org-taskforecast-test.el ends here

;; Local Variables:
;; byte-compile-error-on-warn: t
;; End:
