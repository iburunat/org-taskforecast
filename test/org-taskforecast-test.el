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


;;; org-taskforecast-test.el ends here

;; Local Variables:
;; byte-compile-error-on-warn: t
;; End:
