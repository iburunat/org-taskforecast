;;; org-taskforecast.el --- Manage closed task list and forecast time flow with org-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Hiroki YAMAKAWA

;; Author:  Hiroki YAMAKAWA <s06139@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-id)

;;;; Custom

(defgroup org-taskforecast nil
  "Manage closed task list and forecast time flow with org-mode."
  :group 'org
  :prefix "org-taskforecast")

(defcustom org-taskforecast-dailylist-file "~/org-taskforecast/%Y/%Y-%m-%d.org"
  "A file name which indicates the location to store daily task list.

This string is expanded by `format-time-string'."
  :type 'string
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-default-todo "TODO"
  "A todo state for task link."
  :type 'string
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-day-start 0000
  "A start time of a day.

It is an integer as hhmm.
The value is less than zero, which means yesterday or older.
The value is over than 2359, which means tommorow or more future.

Example of a range of today:
   0300 => 03:00 ~ 27:00 (03:00 of tomorrow)
  -0100 => 23:00 of yesterday ~ 23:00 of today"
  :type 'integer
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))


;;;; Lisp Utility

;;; Debug

(defvar org-taskforecast-enable-assert nil
  "When non-nil, enable `org-taskforecast-assert'.")

(defmacro org-taskforecast-assert (expr &optional message)
  "Assert EXPR.
When EXPR returns nil call error with MESSAGE.
If `org-taskforecast-enable-assert' is nil, this assertion is disabled."
  `(when org-taskforecast-enable-assert
     (unless ,expr
       (error ,(or message
                   (format "Assertion failed: %s" expr))))))

;;; Alist

(defmacro org-taskforecast-defalist (name fields &optional docstring)
  "Define an alist type.
NAME is a name of the alist.
FIELDS is a list of symbols.
DOCSTRING is a documentation of defining alist.

This macro defines following functions:
- Constructor named as NAME
- Type checker named as NAME-type-p

The constructor is defined like below:

    (org-taskforecast-defalist my-abc (a b c))

    Expands to

    (cl-defun my-abc (&key a b c)
      (list (cons 'a a)
            (cons 'b b)
            (cons 'c c)))

To use the constructor like below:

    (my-abc :a 1 :b 2 :c 3)
    => ((a . 1)
        (b . 2)
        (c . 3))

The type checker returns non-nil when the argument is a valid as
the defined alist.
It checks below:
- the argument is an alist
- all of keys of the alist are in FIELDS
- the alist contains all of keys in FIELDS"
  (declare (indent 2) (doc-string 3))
  `(progn
     (defun ,(intern (format "%s-type-p" name)) (x)
       ,(format "Check an alist is valid as `%s'.
Non-nil means valid." name)
       (and (listp x)
            ,@(--map `(assoc ',it x) fields)
            t))

     (cl-defun ,name (&key ,@fields)
       ,(apply #'concat
               `("Make an alist that contains following fields:"
                 ,@(--map (format "\n- %s" it) fields)
                 "\n\n----\n\n"
                 ,docstring))
       (list ,@(--map `(cons ',it ,it) fields)))))

;;; Time

(defun org-taskforecast--encode-hhmm (hhmm day)
  "Return an encoded time from HHMM as a time of DAY.

HHMM is an integer like `org-taskforecast-day-start'.
DAY is an encoded time."
  (org-taskforecast-assert (integerp hhmm))
  (let ((hour (/ hhmm 100))
        (minute (% hhmm 100))
        (time (decode-time day)))
    (setf (decoded-time-hour time) hour
          (decoded-time-minute time) minute)
    (encode-time time)))

(defun org-taskforecast-today (day-start)
  "Get today's date when the day starts at DAY-START.

DAY-START is an integer like `org-taskforecast-day-start'.
This function returns an encoded time as a date of today."
  (org-taskforecast-assert (integerp day-start))
  (let* ((now (current-time))
         (nowd (decode-time now))
         (start-time (org-taskforecast--encode-hhmm day-start now))
         (dsec (time-to-seconds (time-subtract now start-time))))
    (setf (decoded-time-hour nowd) 0
          (decoded-time-minute nowd) 0
          (decoded-time-second nowd) dsec)
    (encode-time nowd)))

;;; File

(defun org-taskforecast-get-dailylist-file (&optional create)
  "Get the path of today's daily task list file.

When CREATE is set, this function creates the file and its directory.

This function depends on:
- `org-taskforecast-dailylist-file' as a file format
- `org-taskforecast-day-start' to determine the date of today"
  (let* ((today (org-taskforecast-today org-taskforecast-day-start))
         (file (expand-file-name
     (format-time-string org-taskforecast-dailylist-file today))))
    (when (and create (not (file-exists-p file)))
      (make-directory (file-name-directory file) t)
      (write-region "" nil file))
    file))

;;; Org-mode

(defmacro org-taskforecast--at-id (id &rest body)
  "Eval BODY at a heading of ID."
  (declare (indent 1))
  `(save-window-excursion
     (save-excursion
       (-let (((file . pos) (org-id-find id)))
         (with-current-buffer (find-file file)
           (save-excursion
             (goto-char pos)
             ,@body))))))

(org-taskforecast-defalist org-taskforecast--task-alist
    (id title)
  "Alist of a task.

The task is a heading linked from daily task list file.
- ID is an id of org-id
- TITLE is a heading title")

(defun org-taskforecast--get-task-by-id (id)
  "Get a task alist by ID.

A returned value is an alist of `org-taskforecast--task-alist'."
  (org-taskforecast--at-id id
    (let ((title (org-get-heading t t t t)))
      (org-taskforecast--task-alist
       :id id
       :title title))))

(defun org-taskforecast--insert-task-link (id file todo)
  "Insert a task link for ID at the end of FILE.

The todo state of the task link heading is set to TODO."
  (-let (((&alist 'title title) (org-taskforecast--get-task-by-id id)))
    (with-current-buffer (find-file file)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (concat "* [[id:" id "][" title "]]\n"))
        (org-todo todo)))))


;;;; General Commands

;;; Registration

(defun org-taskforecast-register-task ()
  "Register a task at point as a task for today."
  (interactive)
  (org-taskforecast--insert-task-link
   (org-id-get-create)
   (org-taskforecast-get-dailylist-file t)
   org-taskforecast-default-todo))



(provide 'org-taskforecast)
;;; org-taskforecast.el ends here
