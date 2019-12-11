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

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'dash)
(require 's)

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

(defun org-taskforecast--normalize-title (title)
  "Normalize a TITLE of a heading."
  (s-replace-all '(("[" . "{") ("]" . "}"))
                 (org-link-display-format title)))

(org-taskforecast-defalist org-taskforecast--task-alist
    (id title)
  "Alist of a task.

The task is a heading linked from daily task list file.
- ID is an id of org-id
- TITLE is a heading title")

(defun org-taskforecast--get-task ()
  "Get a task as an alist.

A returned value is an alist of `org-taskforecast--task-alist'."
  (let ((id (org-id-get-create))
        (title (org-taskforecast--normalize-title
                (substring-no-properties (org-get-heading t t t t)))))
    (org-taskforecast--task-alist
     :id id
     :title title)))

(defun org-taskforecast--get-task-by-id (id)
  "Get a task alist by ID.

A returned value is an alist of `org-taskforecast--task-alist'."
  (org-taskforecast--at-id id
    (org-taskforecast--get-task)))

(org-taskforecast-defalist org-taskforecast--task-link-alist
    (id original-id)
  "Alist of a task link.

It links to a task heading.
- ID is an id of org-id
- ORIGINAL-ID is where this links to")

(defun org-taskforecast--get-link-id (str)
  "Get a link id from STR.

STR is a org-id link string like \"[[id:1234][foo]]\".
If STR is not a org-id link string, this function returns nil."
  (let ((re (rx bos "[[id:" (group (+ (not (any "]")))) "]["
                (+ (not (any "]"))) "]]" eos)))
    (-when-let (((_ id)) (s-match-strings-all re str))
      id)))

(defun org-taskforecast--get-task-link ()
  "Get a task link as an alist.

A returned value is an alist of `org-taskforecast--task-link-alist'.
If the heading is not a task link, this function returns nil."
  (-when-let* ((title (org-get-heading t t t t))
               (original-id (org-taskforecast--get-link-id title))
               ;; Create id when this heading is a task link.
               (id (org-id-get-create)))
    (org-taskforecast--task-link-alist :id id
                                       :original-id original-id)))

(defun org-taskforecast--append-task-link (id file todo)
  "Append a task link for ID to the end of FILE.

The todo state of the task link heading is set to TODO."
  (-let* (((&alist 'title title) (org-taskforecast--get-task-by-id id))
          (normalized (org-taskforecast--normalize-title title)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (concat "* [[id:" id "][" normalized "]]\n"))
        (org-todo todo)))))

(defun org-taskforecast--get-task-links (file)
  "Get a task link list from FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (-non-nil
       (org-map-entries
        (lambda ()
          (org-taskforecast--get-task-link))
        nil 'file)))))


;;;; General Commands

;;; Registration

;;;###autoload
(defun org-taskforecast-register-task ()
  "Register a task at point as a task for today."
  (interactive)
  (org-taskforecast--append-task-link
   (org-id-get-create)
   (org-taskforecast-get-dailylist-file t)
   org-taskforecast-default-todo))


;;;; task-forecast-list mode

(defun org-taskforecast--create-task-list (file)
  "Create a today's task list for a task list file, FILE.

This function returns a string as contents of `org-taskforecast-list-mode'."
  (save-window-excursion
    (let* ((task-links (org-taskforecast--get-task-links file))
           (task-link-lines
            (--map
             (-let* (((&alist 'original-id original-id) it)
                     ((&alist 'title title)
                      (org-taskforecast--get-task-by-id original-id)))
               ;; TODO: Imprement here, now for demo.
               (format "- %s" title))
             task-links)))
      (s-join "\n" task-link-lines))))

(defun org-taskforecast--insert-task-list (file)
  "Insert a today's task list for a task list file, FILE.

This function inserts contents of `org-taskforecast-list-mode'."
  (insert (org-taskforecast--create-task-list file)))

(defvar org-taskforecast-list-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "A key map for `org-taskforecast-list-mode'.")

(define-derived-mode org-taskforecast-list-mode nil "org-taskforecast list"
  "A major-mode to manage today's tasks."
  :group 'org-taskforecast)

(defvar org-taskforecast--list-buffer-name "*org-taskforecast list*"
  "A buffer name for `org-taskforecast-list-mode'.")

(defun org-taskforecast--get-list-buffer ()
  "Get the buffer for `org-taskforecast-list-mode'.

When the buffer is not found, this function returns nil."
  (get-buffer org-taskforecast--list-buffer-name))

(defun org-taskforecast--create-list-buffer (file)
  "Create a buffer for `org-taskforecast-list-mode'.

If the buffer already exists, only returns the buffer.
FILE is a file of a daily task list file."
  (let ((buffer (org-taskforecast--get-list-buffer))
        (file (org-taskforecast-get-dailylist-file)))
    (or buffer
        (with-current-buffer (get-buffer-create
                              org-taskforecast--list-buffer-name)
          (org-taskforecast-list-mode)
          (save-excursion
            (org-taskforecast--insert-task-list file))
          (current-buffer)))))

;;;###autoload
(defun org-taskforecast-list ()
  "Show the buffer of `org-taskforecast-list-mode'."
  (interactive)
  (switch-to-buffer
   (org-taskforecast--create-list-buffer
    (org-taskforecast-get-dailylist-file))))

(defun org-taskforecast-list-refresh ()
  "Refresh `org-taskforecast-list-mode' buffer."
  (interactive)
  ;; TODO: reproduce cursor position
  (let ((file (org-taskforecast-get-dailylist-file)))
    (-if-let (buffer (org-taskforecast--get-list-buffer))
        (with-current-buffer buffer
          (save-excursion
            (erase-buffer)
            (org-taskforecast--insert-task-list file)))
      (user-error
       (concat
        "List buffer (" org-taskforecast--list-buffer-name ") is not found.\n"
        "Please call after `org-taskforecast-list'.")))))



(provide 'org-taskforecast)
;;; org-taskforecast.el ends here
