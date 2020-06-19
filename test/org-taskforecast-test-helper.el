;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)
(require 'dash)
(require 'org)
(require 'org-id)
(require 'org-taskforecast)

;;; deftest

(defvar org-taskforecast-test--work-root
  (f-join (f-dirname (f-this-file)) "work"))

(defvar org-taskforecast-test--auto-clean-work
  (not (member (getenv "TEST_LEAVE_WORK") '("yes" "t"))))

(defun org-taskforecast-test--env-dir (testname)
  (f-join org-taskforecast-test--work-root testname))

(defun org-taskforecast-test--create-env (dir)
  (when (f-directory-p dir)
    (delete-directory dir t))
  (make-directory dir t))

(defun org-taskforecast-test--remove-env (dir)
  (when (and (f-directory-p dir) org-taskforecast-test--auto-clean-work)
    (delete-directory dir t)))

(cl-defmacro org-taskforecast-test-deftest (name args &rest params-and-body)
  "Define ert test which is run in temporary environment.
NAME and ARGS are passed to `ert-deftest'.
PARAMS-AND-BODY accepts keyword parameters and test body.
The keyword parameters are optional.

Keyword parameters:
- CURRENT-TIME
  A string of the current time.
  This macro overrides `current-time' to return that time.

- AGENDA-FILE
  A list like (VAR CONTENTS).
  This macro sets VAR to the file path of an org file for `org-ganeda-files'
  and create that file with CONTENTS before evaluating the test body.

- DAILYLIST-FILE
  A list like (VAR CONTENTS).
  This macro sets VAR to the file path of the daily task list file of
  org-taskforecast and create that file with CONTENTS before evaluating
  the test body.


Example:

    (org-taskforecast-test-deftest TEST-NAME ()
      \"documentation\"
      :current-time \"2020-01-01 00:00:00\"
      :agenda-file (agenda-file \"\\
    * TODO foo
    SCHEDULED: <2020-01-01>\")
      :dailylist-file (dailylist-file \"\")
      TEST-BODY...)

\(fn NAME args &optional docstring &key CURRENT-TIME AGENDA-FILE \
DAILYLIST-FILE &rest BODY)"
  (declare (indent 2))
  (-let* ((testname (symbol-name name))
          (env-root-sym (cl-gensym "env-root-"))
          (dailylist-file-sym (cl-gensym "dailylist-file-"))
          (agenda-file-sym (cl-gensym "agenda-file-"))
          ;; split docstring
          ((docstring . params-and-body)
           (if (stringp (car params-and-body))
               params-and-body
             (cons nil params-and-body)))
          ;; split to key-params and body
          ((key-params . body)
           (cl-loop for (first . rest) on params-and-body by #'cddr
                    if (and (memq first '(:current-time
                                          :dailylist-file
                                          :agenda-file))
                            rest)
                    append (list first (car rest)) into kp
                    else return (cons kp (cons first rest))
                    ;; body not found
                    finally return (cons kp nil)))
          ;; parse keyword parameters
          ((&plist :current-time current-time
                   :dailylist-file dailylist-file
                   :agenda-file agenda-file)
           key-params)
          (time (if current-time
                    (apply #'encode-time (parse-time-string current-time))
                  (current-time)))
          ((dailylist-file-var dailylist-file-content) dailylist-file)
          ((agenda-file-var agenda-file-content) agenda-file))
    (--> `(progn ,@body)
         ;; reset `org-taskforecast-cache-mode'
         `(progn
            (when org-taskforecast-cache-mode
              (org-taskforecast-cache-mode -1))
            ,it
            (when org-taskforecast-cache-mode
              (org-taskforecast-cache-mode -1)))
         ;; override global variables
         `(let (;; org-taskforecast
                (org-taskforecast-dailylist-file ,dailylist-file-sym)
                ;; org-agenda
                (org-agenda-files (list ,agenda-file-sym))
                ;; org-id
                (org-id-locations-file (f-join ,env-root-sym
                                               ".org-id-locations"))
                (org-id-files (list ,dailylist-file-sym ,agenda-file-sym))
                (org-id-extra-files nil)
                (org-id-locations nil)
                ;; Disable org file indentation to help comparison of
                ;; org file contents.
                (org-adapt-indentation nil))
            ,it)
         ;; file contents
         `(progn
            ;; prepare files
            ,@(-non-nil
               (list
                (when dailylist-file
                  `(with-temp-file ,dailylist-file-sym
                     (insert ,dailylist-file-content)))
                (when agenda-file
                  `(with-temp-file ,agenda-file-sym
                     (insert ,agenda-file-content)))))
            ,it
            ;; save files
            (--each (list ,@(-non-nil
                             (list (when dailylist-file dailylist-file-sym)
                                   (when agenda-file agenda-file-sym))))
              (with-current-buffer (find-file-noselect it) (save-buffer))))
         ;; bind file paths
         `(let ,(-non-nil
                 (list
                  (when dailylist-file
                    `(,dailylist-file-var ,dailylist-file-sym))
                  (when agenda-file
                    `(,agenda-file-var ,agenda-file-sym))))
            ,it)
         ;; temporary environment
         `(progn
            (org-taskforecast-test--create-env ,env-root-sym)
            ,it
            (org-taskforecast-test--remove-env ,env-root-sym))
         ;; current-time
         `(cl-letf (((symbol-function #'current-time) (lambda () ',time)))
            ,it)
         ;; temporary symbols for this macro
         `(let* ((,env-root-sym (org-taskforecast-test--env-dir ,testname))
                 (,dailylist-file-sym (f-join ,env-root-sym "dailylist.org"))
                 (,agenda-file-sym (f-join ,env-root-sym "agenda.org")))
            ,it)
         ;; define test
         `(ert-deftest ,name ,args ,@(when docstring (list docstring)),it))))


(provide 'org-taskforecast-test-helper)
;;; org-taskforecast-test-helper.el ends here

;; Local Variables:
;; byte-compile-error-on-warn: t
;; End:
