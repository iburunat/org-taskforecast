;;; org-taskforecast.el --- Manage closed task list and forecast time flow with org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Hiroki YAMAKAWA

;; Author:  Hiroki YAMAKAWA <s06139@gmail.com>
;; URL: https://github.com/HKey/org-taskforecast
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (dash "2.16.0") (dash-functional "2.16.0") (s "1.12.0"))

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
(require 'eieio)
(require 'text-property-search)
(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'org-id)
(require 'dash)
(require 'dash-functional)
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

(defcustom org-taskforecast-list-task-formatters
  (list #'org-taskforecast-list-format-effort
        #'org-taskforecast-list-format-task-start
        #'org-taskforecast-list-format-task-end
        #'org-taskforecast-list-format-link-todo
        #'org-taskforecast-list-format-title)
  "Function list for formatting a task link.

The results of the functions are joind with \" \" and
empty strings are ignored..
The functions should have no parameter.
The functions are obtained information as global variables below:
- `org-taskforecast-list-info-task-link' as an alist of
  `org-taskforecast--task-link-alist'
- `org-taskforecast-list-info-task' as an alist of
  `org-taskforecast--task-alist'
- `org-taskforecast-list-info-today' as an encoded time
- `org-taskforecast-list-info-now' as an encoded time
- `org-taskforecast-list-info-task-start-end-time' as an alist of
  `org-taskforecast--task-link-start-end-time-alist'

Other global variables also are set for formatting:
- `org-taskforecast-day-start'"
  :type '(repeat function)
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-enable-interruption t
  "Non-nil means enable inturruption representation."
  :type 'boolean
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-auto-refresh-list-buffer nil
  "Non-nil means that refresh `org-taskforecast-list-mode' buffer when a task is registered."
  :type 'boolean
  :group 'rog-taskforecast
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
          (decoded-time-minute time) minute
          (decoded-time-second time) 0)
    (encode-time time)))

(defun org-taskforecast--time-as-date (time)
  "Set hour, minute and second of TIME to zero.

TIME is an encoded time.
A returned value is an encoded time."
  (let ((decoded (decode-time time)))
    (setf (decoded-time-hour decoded) 0
          (decoded-time-minute decoded) 0
          (decoded-time-second decoded) 0)
    (encode-time decoded)))

(defun org-taskforecast--today (time day-start)
  "Get today's date of TIME when the day starts at DAY-START.

TIME is an encoded time.
DAY-START is an integer like `org-taskforecast-day-start'.
This function returns an encoded time as a date of today."
  (org-taskforecast-assert (integerp day-start))
  (let* ((decoded (decode-time time))
         (start-time (org-taskforecast--encode-hhmm day-start time))
         (dsec (time-to-seconds (time-subtract time start-time))))
    (setf (decoded-time-hour decoded) 0
          (decoded-time-minute decoded) 0
          (decoded-time-second decoded) dsec)
    (org-taskforecast--time-as-date
     (encode-time decoded))))

(defun org-taskforecast-today ()
  "Get today's date of now.

This function depends on:
- `org-taskforecast-day-start'"
  (org-taskforecast--today (current-time)
                           org-taskforecast-day-start))

(org-taskforecast-defalist org-taskforecast--hhmm-alist (hour minute)
  "A pair of hour and minute.

HOUR and MINUTE are integers.")

(defun org-taskforecast--time-to-hhmm (time today)
  "Convert TIME to hour and minute as time of TODAY."
  (let* ((today (org-taskforecast--time-as-date today))
         (dsec (floor (time-to-seconds (time-subtract time today))))
         (dmin (/ dsec 60))
         (hour (/ dmin 60))
         (minute (% dmin 60)))
    (org-taskforecast-assert (<= 0 dsec))
    (org-taskforecast--hhmm-alist :hour hour :minute minute)))

(defun org-taskforecast--today-p (time today day-start)
  "Return non-nil if TIME is in range of TODAY.

- TIME is an encoded time
- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((start (org-taskforecast--encode-hhmm day-start today))
        (end (org-taskforecast--encode-hhmm (+ day-start 2400) today)))
    (and (time-less-p time end)
         (or (time-equal-p start time)
             (time-less-p start time)))))

;;; File

(defun org-taskforecast-get-dailylist-file (today)
  "Get the path of today's daily task list file for TODAY.

This function depends on:
- `org-taskforecast-dailylist-file' as a file format
- `org-taskforecast-day-start' to determine the date of today"
  (expand-file-name
   (format-time-string org-taskforecast-dailylist-file today)))

;;; Org-mode

(defconst org-taskforecast--task-link-effective-start-time-prop-name
  "ORG_TASKFORECAST_TASK_LINK_EFFECTIVE_START_TIME"
  "Property name of an effective start time of a task link.")

(defconst org-taskforecast--task-link-effective-end-time-prop-name
  "ORG_TASKFORECAST_TASK_LINK_EFFECTIVE_END_TIME"
  "Property name of an effective end time of a task link.")

(defun org-taskforecast--get-task-link-effective-start-time ()
  "Get the task link's effective start time property from a heading.

A returned value is an encoded time."
  (-some-->
      (org-entry-get
       nil
       org-taskforecast--task-link-effective-start-time-prop-name)
    (org-parse-time-string it)
    (encode-time it)))

(defun org-taskforecast--get-task-link-effective-end-time ()
  "Get the task link's effective end time property from a heading.

A returned value is an encoded time."
  (-some-->
      (org-entry-get
       nil
       org-taskforecast--task-link-effective-end-time-prop-name)
    (org-parse-time-string it)
    (encode-time it)))

(defun org-taskforecast--set-task-link-effective-start-time (time)
  "Set the task link's effective start time property to TIME.

TIME is an encoded time."
  (org-entry-put nil
                 org-taskforecast--task-link-effective-start-time-prop-name
                 (format-time-string (org-time-stamp-format t t) time)))

(defun org-taskforecast--set-task-link-effective-end-time (time)
  "Set the task link's effective end time property to TIME.

TIME is an encoded time."
  (org-entry-put nil
                 org-taskforecast--task-link-effective-end-time-prop-name
                 (format-time-string (org-time-stamp-format t t) time)))

(defmacro org-taskforecast--at-id (id &rest body)
  "Eval BODY at a heading of ID."
  (declare (indent 1) (debug t))
  `(-let (((file . pos) (org-id-find ,id)))
     (with-current-buffer (find-file-noselect file)
       (save-excursion
         (goto-char pos)
         ;; To parse with org element api properly
         ;; even when the heading is folded and invisible.
         (org-show-context)
         ,@body))))

(defun org-taskforecast--normalize-title (title)
  "Normalize a TITLE of a heading."
  (s-replace-all '(("[" . "{") ("]" . "}"))
                 (org-link-display-format title)))

(defun org-taskforecast--parse-heading-without-subtree ()
  "Parse heading at point without subtree by org element api."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region
       (progn (outline-back-to-heading) (point))
       (progn (outline-next-heading) (point)))
      (org-element-parse-buffer))))

(defclass org-taskforecast--clock ()
  ((start
    :initarg :start
    :reader org-taskforecast--clock-start
    :documentation
    "A start time of a clock of a task as an encoded time.")
   (end
    :initarg :end
    :reader org-taskforecast--clock-end
    :documentation
    "An end time of a clock of a task as an encoded time."))
  :documentation
  "A clock data.")

(defun org-taskforecast--clock-duration (clock)
  "Duration of CLOCK as an encoded time.

CLOCK is an instance of `org-taskforecast--clock'."
  (time-subtract (org-taskforecast--clock-end clock)
                 (org-taskforecast--clock-start clock)))

(defun org-taskforecast--clock-start-less-p (a b)
  "Compare start-times of A and B by `time-less-p'.

A and B are instances of `org-taskforecast--clock'."
  (time-less-p (org-taskforecast--clock-start a)
               (org-taskforecast--clock-start b)))

(defun org-taskforecast--timestamp-start-time (timestamp)
  "Get an encoded time of the start time of TIMESTAMP.

TIMESTAMP is an element of timestamp of org element api.
The second part of a returned time is set to zero.
If hour and minute part do not exist, they are set to zero."
  (encode-time
   0
   (or (org-element-property :minute-start timestamp) 0)
   (or (org-element-property :hour-start timestamp) 0)
   (org-element-property :day-start timestamp)
   (org-element-property :month-start timestamp)
   (org-element-property :year-start timestamp)))

(defun org-taskforecast--timestamp-end-time (timestamp)
  "Get an encoded time of the end time of TIMESTAMP.

TIMESTAMP is an element of timestamp of org element api.
The second part of a returned time is set to zero.
If hour and minute part do not exist, they are set to zero."
  (encode-time
   0
   (or (org-element-property :minute-end timestamp) 0)
   (or (org-element-property :hour-end timestamp) 0)
   (org-element-property :day-end timestamp)
   (org-element-property :month-end timestamp)
   (org-element-property :year-end timestamp)))

(org-taskforecast-defalist org-taskforecast--scheduled-alist
    (start-time date-only-p repeatp)
  "Alst of a SCHEDULED property of a task.

- START-TIME is an encoded time of the start time of schedule
- DATE-ONLY-P is a boolean, it is non-nil if the start time stamp has
  no hour and minute
- REPEATP is a boolean, it is non-nil if the time stamp has a repeater")

(defun org-taskforecast--get-scheduled-from-timestamp (timestamp)
  "Get a scheduled information from TIMESTAMP.

TIMESTAMP is a timestamp element of a scheduled property of a heading
of org element api.
This function returns an alist of `org-taskforecast--scheduled-alist'."
  (let ((start-time (org-taskforecast--timestamp-start-time timestamp))
        (date-only-p (not (or (org-element-property :hour-start timestamp)
                              (org-element-property :minute-start timestamp))))
        (repeatp (and (org-element-property :repeater-type timestamp) t)))
    (org-taskforecast--scheduled-alist
     :start-time start-time
     :date-only-p date-only-p
     :repeatp repeatp)))

(defclass org-taskforecast--deadline ()
  ((time
    :initarg :time
    :reader org-taskforecast--deadline-time
    :documentation
    "An encoded time of a deadline.")
   (date-only-p
    :initarg :date-only-p
    :reader org-taskforecast--deadline-date-only-p
    :type boolean
    :documentation
    "Non-nil means the time stamp of a deadline has no hour and minute sections.")
   (repeatp
    :initarg :repeatp
    :reader org-taskforecast--deadline-repeat-p
    :type boolean
    :documentation
    "Non-nil means the time stamp of a deadline has a repeater."))
  :documentation
  "A deadline data.")

(defun org-taskforecast--get-deadline-from-timestamp (timestamp)
  "Get a deadline information from TIMESTAMP.

TIMESTAMP is a timestamp element of a deadline property of a heading
of org element api.
This function returns an instance of `org-taskforecast--deadline'."
  (let ((time (org-taskforecast--timestamp-start-time timestamp))
        (date-only-p (not (or (org-element-property :hour-start timestamp)
                              (org-element-property :minute-start timestamp))))
        (repeatp (and (org-element-property :repeater-type timestamp) t)))
    (org-taskforecast--deadline
     :time time
     :date-only-p date-only-p
     :repeatp repeatp)))

(org-taskforecast-defalist org-taskforecast--task-alist
    (id title effort status clocks todo todo-type scheduled deadline)
  "Alist of a task.

The task is a heading linked from daily task list file.
- ID is an id of org-id
- TITLE is a heading title
- EFFORT is a value of effort property
- STATUS is a symbol of todo, running and done
- CLOCKS is a list of clock data, each element is an instance of
  `org-taskforecast--clock'
- TODO is a string of a todo state (optional)
- TODO-TYPE is a symbol of a type of todo (optional)
- SCHEDULED is a schedule infomaton as an alist of
  `org-taskforecast--scheduled-alist' (optional)
- DEADLINE is a deadline information as an instance of
  `org-taskforecast--deadline' (optional)")

(defun org-taskforecast--get-clock-from-element (element)
  "Get a clock from ELEMENT.

ELEMENT is a clock element of org element api."
  (let* ((timestamp (org-element-property :value element))
         (runnigp (eq 'running (org-element-property :status element)))
         (start (org-taskforecast--timestamp-start-time timestamp))
         (end (and (not runnigp)
                   (org-taskforecast--timestamp-end-time timestamp))))
    (org-taskforecast--clock :start start :end end)))

(defun org-taskforecast--get-task ()
  "Get a task as an alist.

A returned value is an alist of `org-taskforecast--task-alist'."
  (save-excursion
    ;; go to heading line for `org-element-at-point' to get a headline element
    (org-back-to-heading)
    (let* ((id (org-id-get-create))
           (element (org-element-at-point))
           (title (org-taskforecast--normalize-title
                   (substring-no-properties (org-element-property :title element))))
           (effort (org-entry-get nil org-effort-property))
           (todo (org-element-property :todo-keyword element))
           (todo-type (org-element-property :todo-type element))
           (scheduled (-some--> (org-element-property :scheduled element)
                        (org-taskforecast--get-scheduled-from-timestamp it)))
           (deadline (-some--> (org-element-property :deadline element)
                       (org-taskforecast--get-deadline-from-timestamp it)))
           (helement (org-taskforecast--parse-heading-without-subtree))
           (running-p (-contains-p
                       (org-element-map helement 'clock
                         (lambda (x) (org-element-property :status x)))
                       'running))
           (clocks (org-element-map helement 'clock
                     #'org-taskforecast--get-clock-from-element))
           (status (cond
                    ((and (eq todo-type 'todo) running-p) 'running)
                    ((and (eq todo-type 'todo) (not running-p)) 'todo)
                    ((eq todo-type 'done) 'done)
                    (t (error "Not a task heading")))))
      (org-taskforecast--task-alist
       :id id
       :title title
       :effort effort
       :status status
       :clocks clocks
       :todo todo
       :todo-type todo-type
       :scheduled scheduled
       :deadline deadline))))

(defun org-taskforecast--get-task-by-id (id)
  "Get a task alist by ID.

A returned value is an alist of `org-taskforecast--task-alist'."
  (org-taskforecast--at-id id
    (org-taskforecast--get-task)))

(org-taskforecast-defalist org-taskforecast--task-link-alist
    (id original-id effective-start-time effective-end-time)
  "Alist of a task link.

It links to a task heading.
- ID is an id of org-id
- ORIGINAL-ID is where this links to
- EFFECTIVE-START-TIME is an encoded time when the task link is
  effective after (optional)
- EFFECTIVE-END-TIME is an encoded time when the task link is
  effective before (optional)")

(defun org-taskforecast--get-link-id (str)
  "Get a link id from STR.

STR is a org-id link string like \"[[id:1234][foo]]\".
If STR is not a org-id link string, this function returns nil."
  (let ((re (rx bos "[[id:" (group (+ (not (any "]")))) "]["
                (+ (not (any "]"))) "]]" eos)))
    (-when-let (((_ id)) (s-match-strings-all re str))
      id)))

(defun org-taskforecast--get-task-todo-state-for-today (task date day-start)
  "Get todo state of TASK for today.

This function returns a symbol, todo or done.
- TASK is an alist of `org-taskforecast--task-alist'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (org-taskforecast-assert (org-taskforecast--task-alist-type-p task))
  (-let* (((&alist 'todo-type todo-type 'scheduled scheduled
                   'deadline deadline) task)
          ((&alist 'start-time stime_ 'repeatp srepeatp
                   'date-only-p sdate-only-p) scheduled)
          (stime (if (and stime_ sdate-only-p)
                     (org-taskforecast--encode-hhmm day-start stime_)
                   stime_))
          (dtime (when deadline
                   (let ((time (org-taskforecast--deadline-time deadline)))
                     (if (org-taskforecast--deadline-date-only-p deadline)
                         (org-taskforecast--encode-hhmm day-start time)
                       time))))
          (repeatp (or (and scheduled srepeatp)
                       (and deadline (org-taskforecast--deadline-repeat-p deadline))))
          (times (-non-nil (list (and scheduled stime) (and deadline dtime))))
          (next-day-start
           (org-taskforecast--encode-hhmm (+ day-start 2400) date)))
    (unless todo-type
      (error "Task is not a todo heading"))
    (if (eq todo-type 'done)
        'done
      (org-taskforecast-assert (eq todo-type 'todo))
      (if (not repeatp)
          'todo
        (org-taskforecast-assert (not (null times)))
        (if (--some (time-less-p it next-day-start) times)
            'todo
          'done)))))

(defun org-taskforecast--get-task-link-todo-state-for-today (task-link date day-start)
  "Get todo state of TASK-LINK for today.

This function returns a symbol, todo or done.
- TASK-LINK is an alist of `org-taskforecast--task-link-alist'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (org-taskforecast-assert (org-taskforecast--task-link-alist-type-p task-link))
  (-let* (((&alist 'original-id original-id
                   'effective-end-time effective-end-time)
           task-link)
          (task (org-taskforecast--get-task-by-id original-id)))
    (if effective-end-time
        ;; interrupted if efective-end-time exists
        'done
      (org-taskforecast--get-task-todo-state-for-today task date day-start))))

(defun org-taskforecast--get-task-link ()
  "Get a task link as an alist.

A returned value is an alist of `org-taskforecast--task-link-alist'.
If the heading is not a task link, this function returns nil."
  (save-excursion
    ;; Prevent error when there is no heading in a buffer.
    (unless (org-before-first-heading-p)
      ;; go to heading line for `org-element-at-point' to get a headline element
      (org-back-to-heading))
    (let* ((element (org-element-at-point))
           (title (org-element-property :title element))
           (effective-start-time
            (org-taskforecast--get-task-link-effective-start-time))
           (effective-end-time
            (org-taskforecast--get-task-link-effective-end-time)))
      (-when-let* ((original-id (org-taskforecast--get-link-id title))
                   ;; Create id when this heading is a task link.
                   (id (org-id-get-create)))
        (org-taskforecast--task-link-alist
         :id id
         :original-id original-id
         :effective-start-time effective-start-time
         :effective-end-time effective-end-time)))))

(defun org-taskforecast--get-task-link-by-id (id)
  "Get a task link alist by ID.

A returned value is an alist of `org-taskforecast--task-link-alist'."
  (org-taskforecast--at-id id
    (org-taskforecast--get-task-link)))

(defun org-taskforecast--append-task-link (id file)
  "Append a task link for ID to the end of FILE.

This function returns an ID of the new task link."
  (-let* (((&alist 'title title) (org-taskforecast--get-task-by-id id))
          (normalized (org-taskforecast--normalize-title title)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (concat "* [[id:" id "][" normalized "]]\n"))
        (prog1
            (org-id-get-create)
          ;; The reason of saving buffer here is to find headnig
          ;; by org-id properly, org-id doesn't return control when
          ;; the file doesn't exist.
          (save-buffer))))))

(defun org-taskforecast--append-task-link-maybe (id file date day-start)
  "Append a task link for ID to the end of FILE.

If a todo task link corresponding to ID already exists,
this function does nothing.
This function returns an ID of the task link which was appended or already
exists corresponding to the task.

- ID is a task id
- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (--> (org-taskforecast--get-task-links-for-task id file)
       (--filter
        (eq 'todo
            (org-taskforecast--get-task-link-todo-state-for-today
             it date day-start))
        it)
       (-if-let ((&alist 'id link-id) (-first-item it))
           link-id
         (org-taskforecast--append-task-link id file))))

(defun org-taskforecast--get-first-todo-task-link (file date day-start)
  "Get the first todo task link in FILE.

A returned value is an alist of `org-taskforecast--task-link-alist'.
If a first todo task is not found, this function returns nil.

- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char
       (org-taskforecast--get-todo-link-head-pos file date day-start))
      ;; `org-taskforecast--get-todo-link-head-pos' returns the end
      ;; of buffer if there is no todo task link.
      ;; So checking that the returned task link is really todo task link.
      (let ((task-link (org-taskforecast--get-task-link)))
        (when (and task-link
                   (eq 'todo
                       (org-taskforecast--get-task-link-todo-state-for-today
                        task-link date day-start)))
          task-link)))))

(defun org-taskforecast--get-clocks-in-range (clocks start end)
  "Get clocks between START and END from CLOCKS.

- CLOCKS is a list of instances of `org-taskforecast--clock'
- START is an encoded time
- END is an encoded time"
  (--filter
   (let ((cstart (org-taskforecast--clock-start it)))
     (and (not (time-less-p cstart start))
          (time-less-p cstart end)))
   clocks))

(defun org-taskforecast--has-effective-clock (task-link date day-start)
  "Non-nil means TASK-LINK has some effective clocks.

An effective clock is a clock information that clocked today.
If the task link has effective start/end time, an effective clock satisfies
the following conditions:
- the clock was started after the effective start time
- the clock was ended before the effective end time

- TASK-LINK is an alist of `org-taskforecast--task-link-alist'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (org-taskforecast-assert
   (org-taskforecast--task-link-alist-type-p task-link))
  (-let* (((&alist 'original-id original-id
                   'effective-start-time effective-start-time
                   'effective-end-time effective-end-time)
           task-link)
          ((&alist 'clocks clocks)
           (org-taskforecast--get-task-by-id original-id)))
    (--some
     (let ((start (org-taskforecast--clock-start it))
           (today-start (org-taskforecast--encode-hhmm day-start date))
           (next-day-start (org-taskforecast--encode-hhmm
                            (+ day-start 2400) date)))
       (and (not (time-less-p start today-start))
            (time-less-p start next-day-start)
            (or (null effective-start-time)
                (not (time-less-p start effective-start-time)))
            (or (null effective-end-time)
                (time-less-p start effective-end-time))))
     clocks)))

(defun org-taskforecast--get-task-link-effort (task-link date day-start)
  "Get effort value of TASK-LINK.

A returned value is an effort second.

- TASK-LINK is an alist of `org-taskforecast--task-link-alist'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (-let* (((&alist 'original-id original-id
                   'effective-start-time effective-start-time
                   'effective-end-time effective-end-time)
           task-link)
          ((&alist 'clocks clocks 'effort effort)
           (org-taskforecast--get-task-by-id original-id))
          (effort-sec (org-taskforecast--effort-to-second effort))
          (time-greater-p (-flip #'time-less-p))
          (today-start (org-taskforecast--encode-hhmm day-start date))
          (range-start
           (-max-by time-greater-p
                    (-non-nil (list today-start effective-start-time))))
          (range-end
           (-min-by time-greater-p
                    (-non-nil
                     (list effective-end-time
                           (org-taskforecast--encode-hhmm (+ day-start 2400)
                                                          date)))))
          (dsec (-compose #'time-to-seconds
                          #'org-taskforecast--clock-duration))
          (used-sec-before
           (--> (org-taskforecast--get-clocks-in-range
                 clocks today-start range-start)
                (-map dsec it)
                (-sum it)))
          (remaining-effort-sec
           (when effort-sec (max (- effort-sec used-sec-before) 0)))
          (used-sec
           (--> (org-taskforecast--get-clocks-in-range
                 clocks range-start range-end)
                (-map dsec it)
                (-sum it))))
    (-some-->
        (cond ((null effort-sec) nil)
              (effective-end-time (min remaining-effort-sec used-sec))
              (t remaining-effort-sec))
      (floor it))))

(defun org-taskforecast--format-second-to-hhmm (second)
  "Format SECOND to HH:MM style string."
  (format "%d:%02d" (/ second 3600) (/ (% second 3600) 60)))

(defun org-taskforecast--split-task-link (link-id time file)
  "Split a task link of LINK-ID on FILE as interrupted at TIME."
  (-let* (((&alist 'original-id original-id)
           (org-taskforecast--get-task-link-by-id link-id))
          (new-link-id (org-taskforecast--append-task-link original-id file))
          (new-link-heading (org-taskforecast--cut-heading-by-id new-link-id)))
    (org-taskforecast--at-id link-id
      (org-taskforecast--set-task-link-effective-end-time time)
      (outline-next-heading)
      (insert new-link-heading)
      ;; Now the corsor is at the next heading if it exists.
      ;; So move the cursor backward.
      (forward-char -1)
      (org-taskforecast--set-task-link-effective-start-time time))))

(defun org-taskforecast--push-task-link-maybe (id file date day-start)
  "Add a task link for ID to the head of todo task links in FILE.

If a task link corresponding to ID already exists, this function moves it.
If the existing task link is done, this function does not move it.
This function returns an ID of the task link corresponding to the task.

If the first todo task link of the task link list has effective clocks and
its task is not the pushed task, this function splits the first todo task
link as interrupted.
This feature is enabled while `org-taskforecast-enable-interruption'
is non-nil.

- ID is a task id
- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  ;; interruption
  (when org-taskforecast-enable-interruption
    (-when-let* ((first-todo-task-link
                  (org-taskforecast--get-first-todo-task-link
                   file date day-start))
                 ((&alist 'original-id original-id 'id link-id)
                  first-todo-task-link))
      (when (and (not (equal id original-id))
                 (org-taskforecast--has-effective-clock
                  first-todo-task-link
                  date
                  day-start))
        (org-taskforecast--split-task-link link-id (current-time) file))))
  ;; TODO: consider a case that the task of ID is already done
  (let* ((link-id (org-taskforecast--append-task-link-maybe
                   id file date day-start))
         (task-link (org-taskforecast--get-task-link-by-id link-id))
         (todo-type (org-taskforecast--get-task-link-todo-state-for-today
                     task-link date day-start)))
    (when (eq todo-type 'todo)
      (org-taskforecast--move-task-link-to-todo-head link-id file date day-start))
    link-id))

(defun org-taskforecast--map-headings (fn)
  "Call FN on headings in the current buffer.

This function returns a list of results of FN.

Why use this function instead of `org-map-entries' is to avoid asking
about non-existent agenda file by `org-check-agenda-file' when
the file of the current buffer doesn't exist."
  (let* (results
         (f (lambda () (push (funcall fn) results))))
    (org-map-region f (point-min) (point-max))
    (nreverse results)))

(defun org-taskforecast--get-task-links (file)
  "Get a task link list from FILE."
  (with-current-buffer (find-file-noselect file)
    (-non-nil
     (org-taskforecast--map-headings #'org-taskforecast--get-task-link))))

(defun org-taskforecast--effort-to-second (effort-str)
  "Convert string of effort property to second.

EFFORT-STR is a string of a value of an effort property.
If effort-str invalid, this function returns nil."
  (-let* ((re (rx bos (? (group (+ num)) ":") (group (+ num)) eos))
          (((_ h m)) (s-match-strings-all re effort-str)))
    ;; Effort property sometimes has no colon format like "0".
    (when m
      (+ (* 60 60 (if h (string-to-number h) 0))
         (* 60 (string-to-number m))))))

(org-taskforecast-defalist org-taskforecast--task-link-start-end-time-alist
    (start end start-estimated-p end-estimated-p overrunp)
  "An information of start and end time of a task.

- START is an encoded time that indicates the start time of the task of today.
  If the start time is not found, the value will be an estimated time.
- END is an encoded time that indicates the end time of the task of today.
  If the end time is not found, the value will be an estimated time.
- START-ESTIMATED-P is a boolean.
  If the start time is estimated, its value is non-nil.
- END-ESTIMATED-P is a boolean.
  If the end time is estimated, its value is non-nil.
- OVERRUNP is a boolean.
  If the end time is over the time of the start time plus effort,
  its value is non-nil.")

(defun org-taskforecast--get-task-link-start-end-time (task-link date day-start &optional start-after now)
  "Get the start and end time of a TASK-LINK.

This function returns a `org-taskforecast--task-link-start-end-time-alist'.

- TASK-LINK is an alist of `org-taskforecast--task-link-alist'
- DATE is an encoded time as the date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- START-AFTER is an encoded time (optional).
  If it is set, ignore clocks whose start time is earlier than it.
- NOW is an encoded time (optional).
  If it is set, use it instead of an estimated time of start or end
  when the estimated time is earlier than it."
  (org-taskforecast-assert
   (org-taskforecast--task-link-alist-type-p task-link))
  (-let* ((day-start-time
           (org-taskforecast--encode-hhmm day-start date))
          (next-day-start-time
           (org-taskforecast--encode-hhmm (+ day-start 2400) date))
          ((&alist 'original-id original-id
                   'effective-start-time effective-start-time
                   'effective-end-time effective-end-time)
           task-link)
          (todo (org-taskforecast--get-task-link-todo-state-for-today
                 task-link date day-start))
          (task (org-taskforecast--get-task-by-id original-id))
          (effort-sec (or
                       (org-taskforecast--get-task-link-effort
                        task-link date day-start)
                       0))
          ((&alist 'clocks clocks) task)
          (clock-start-greater-p
           (-flip #'org-taskforecast--clock-start-less-p))
          (time-greater-p (-flip #'time-less-p))
          (start-after
           (-max-by time-greater-p
                    (-non-nil
                     (list start-after day-start-time effective-start-time))))
          (end-before
           (-min-by time-greater-p
                    (-non-nil
                     (list next-day-start-time effective-end-time))))
          (target-clocks
           (org-taskforecast--get-clocks-in-range clocks
                                                  start-after
                                                  end-before))
          (start-time
           (-some--> target-clocks
             (-min-by clock-start-greater-p it)
             (org-taskforecast--clock-start it)))
          (end-time
           (-some--> target-clocks
             (-max-by clock-start-greater-p it)
             (org-taskforecast--clock-end it)))
          (start-estimated-p (null start-time))
          (end-estimated-p (or (null end-time) (eq todo 'todo)))
          (start
           (cond ((and start-estimated-p (eq todo 'todo) now)
                  (-max-by time-greater-p (list start-after now)))
                 (start-estimated-p start-after)
                 (t start-time)))
          (start-plus-effort
           (time-add start (seconds-to-time effort-sec)))
          (end
           (cond ((and end-estimated-p (eq todo 'todo) now)
                  (-max-by time-greater-p (list start-plus-effort now)))
                 ((and end-estimated-p (eq todo 'done)) start)
                 (t end-time)))
          (overrunp (time-less-p start-plus-effort end)))
    (org-taskforecast--task-link-start-end-time-alist
     :start start
     :end end
     :start-estimated-p start-estimated-p
     :end-estimated-p end-estimated-p
     :overrunp overrunp)))

(defun org-taskforecast--get-task-links-for-task (task-id file)
  "Get task links for the task of TASK-ID in FILE.

- TASK-ID is a string
- FILE is a today's daily task list file name"
  (--filter
   (-let (((&alist 'original-id original-id) it))
     (string= task-id original-id))
   (org-taskforecast--get-task-links file)))

(defun org-taskforecast--get-todo-link-head-pos (file date day-start)
  "Get the point of head of todo task links in FILE.

- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let ((pos nil))
    (with-current-buffer (find-file-noselect file)
      (org-taskforecast--map-headings
       (lambda ()
         (unless pos
           (--> (org-taskforecast--get-task-link)
                (org-taskforecast--get-task-link-todo-state-for-today
                 it date day-start)
                (when (eq it 'todo)
                  (setq pos (point)))))))
      (or pos (point-max)))))

(defun org-taskforecast--cut-heading-by-id (id)
  "Cut a heading by ID.

Return a string of the heading.
When this function failed, returns nil."
  (org-taskforecast--at-id id
    (save-excursion
      (-when-let* ((helement (org-element-at-point))
                   (begin (org-element-property :begin helement))
                   (end (org-element-property :end helement)))
        (prog1
            (buffer-substring begin end)
          (delete-region begin end))))))

(defun org-taskforecast--move-task-link-to-todo-head (link-id file date day-start)
  "Move a task link of LINK-ID to the head of todo task links of FILE.

- ID is an id of org-id of a task link
- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (unless (org-taskforecast--at-id link-id (org-taskforecast--get-task-link))
    (error "Not a task link ID: %s" link-id))
  (let ((task-link (org-taskforecast--cut-heading-by-id link-id))
        (head (org-taskforecast--get-todo-link-head-pos file date day-start)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char head)
        (insert task-link)))))


;;;; General Commands

;;; Registration

(defmacro org-taskforecast--at-agenda-heading (&rest body)
  "Eval BODY at a heading of the current line of `org-agenda' buffer."
  (declare (indent 0) (debug t))
  `(if (not (eq 'org-agenda-mode major-mode))
       (error "Not an org-agenda buffer")
     (let* ((marker (org-get-at-bol 'org-hd-marker))
            (buffer (marker-buffer marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char marker)
             (org-show-context)
             ,@body))))))

;;;###autoload
(defun org-taskforecast-register-task ()
  "Register a task at point as a task for today.

When the task is already registered, this command does nothing."
  (interactive)
  (let ((id (if (eq 'org-agenda-mode major-mode)
                (org-taskforecast--at-agenda-heading
                  (org-id-get-create))
              (org-id-get-create)))
        (file (org-taskforecast-get-dailylist-file (org-taskforecast-today))))
    (if (org-taskforecast--get-task-links-for-task id file)
        (message "The task is already registered.")
      (org-taskforecast--append-task-link id file)
      (org-taskforecast--list-refresh-maybe))))


;;;; task-forecast-list mode

(defvar org-taskforecast--list-task-link-property 'task-link
  "A property symbol for a task link data to propertize string.")

(defun org-taskforecast--list-propertize-link-data (str task-link)
  "Put a task link data, TASK-LINK, into STR."
  (org-taskforecast-assert (org-taskforecast--task-link-alist-type-p task-link))
  (propertize str
              org-taskforecast--list-task-link-property
              task-link))

(defun org-taskforecast--list-get-task-link-at-point ()
  "Get a task link data via text property from current point.

When there is no task link data, this function returns nil."
  (get-text-property (point)
                     org-taskforecast--list-task-link-property))

(defvar org-taskforecast-list-info-task-link nil
  "This variable is used to pass a task link data to formatters.

This value will be a `org-taskforecast--task-link-alist'.
See `org-taskforecast-list-task-formatters' for more detail.")

(defvar org-taskforecast-list-info-task nil
  "This variable is used to pass a task data to formatters.

This value will be a `org-taskforecast--task-alist'.
See `org-taskforecast-list-task-formatters' for more detail.")

(defvar org-taskforecast-list-info-today nil
  "This variable is used to pass a date of today to formatters.

This value will be an encoded time.
Its hour, minute and second are set to zero.
See `org-taskforecast-list-task-formatters' for more detail.")

(defvar org-taskforecast-list-info-now nil
  "This variable is used to pass the current time to formatters.

This value will be an encoded time.
See `org-taskforecast-list-task-formatters' for more detail.")

(defvar org-taskforecast-list-info-task-start-end-time nil
  "This variable is used to pass the start and end time to formatters.

This value will be an alist of
`org-taskforecast--task-link-start-end-time-alist'.
See `org-taskforecast-list-task-formatters' for more detail.")

(defun org-taskforecast-list-format-effort ()
  "Format effort property of a task.

This function is used for `org-taskforecast-list-task-formatters'."
  (org-taskforecast-assert
   (org-taskforecast--task-alist-type-p org-taskforecast-list-info-task))
  (let ((effort (org-taskforecast--get-task-link-effort
                 org-taskforecast-list-info-task-link
                 org-taskforecast-list-info-today
                 org-taskforecast-day-start)))
    (format "%5s"
            (if effort
                (org-taskforecast--format-second-to-hhmm effort)
              "-:--"))))

(defun org-taskforecast-list-format-task-start ()
  "Format time when a task has been started.

This function is used for `org-taskforecast-list-task-formatters'."
  (org-taskforecast-assert
   (org-taskforecast--task-alist-type-p org-taskforecast-list-info-task))
  (org-taskforecast-assert
   (let ((decoded (decode-time org-taskforecast-list-info-today)))
     (and (= (decoded-time-hour decoded)
             (decoded-time-minute decoded)
             (decoded-time-second decoded)
             0))))
  (-let* (((&alist 'start start 'start-estimated-p start-estimated-p)
           org-taskforecast-list-info-task-start-end-time)
          ((&alist 'hour hour 'minute minute)
           (org-taskforecast--time-to-hhmm
            start
            org-taskforecast-list-info-today)))
    (org-taskforecast-assert (--all-p (>= it 0) (list hour minute)))
    (propertize (format "%02d:%02d" hour minute)
                ;; TODO: define face
                'face (if start-estimated-p 'org-scheduled 'default))))

(defun org-taskforecast-list-format-task-end ()
  "Format time when a task has been closed.

This function is used for `org-taskforecast-list-task-formatters'."
  (org-taskforecast-assert
   (org-taskforecast--task-alist-type-p org-taskforecast-list-info-task))
  (org-taskforecast-assert
   (let ((decoded (decode-time org-taskforecast-list-info-today)))
     (and (= (decoded-time-hour decoded)
             (decoded-time-minute decoded)
             (decoded-time-second decoded)
             0))))
  (-let* ((todo-type
           (org-taskforecast--get-task-link-todo-state-for-today
            org-taskforecast-list-info-task-link
            org-taskforecast-list-info-today
            org-taskforecast-day-start))
          ((&alist 'end end
                   'end-estimated-p end-estimated-p
                   'overrunp overrunp_)
           org-taskforecast-list-info-task-start-end-time)
          (overrunp (and end-estimated-p
                         (eq todo-type 'todo)
                         overrunp_))
          ((&alist 'hour hour 'minute minute)
           (org-taskforecast--time-to-hhmm
            (if overrunp org-taskforecast-list-info-now end)
            org-taskforecast-list-info-today)))
    (propertize (format "%02d:%02d" hour minute)
                ;; TODO: define face
                'face (cond
                       (overrunp 'org-warning)
                       (end-estimated-p 'org-scheduled)
                       (t 'default)))))

(defun org-taskforecast-list-format-link-todo ()
  "Format task link's todo state.

This function is used for `org-taskforecast-list-task-formatters'."
  (org-taskforecast-assert
   (org-taskforecast--task-link-alist-type-p
    org-taskforecast-list-info-task-link))
  (let ((todo-type
         (org-taskforecast--get-task-link-todo-state-for-today
          org-taskforecast-list-info-task-link
          org-taskforecast-list-info-today
          org-taskforecast-day-start)))
    (propertize (cl-case todo-type
                  ('todo "TODO")
                  ('done "DONE"))
                ;; TODO: define face
                'face (cl-case todo-type
                        ('todo 'org-todo)
                        ('done 'org-done)))))

(defun org-taskforecast-list-format-title ()
  "Format task's title.

This function is used for `org-taskforecast-list-task-formatters'."
  (org-taskforecast-assert
   (org-taskforecast--task-alist-type-p org-taskforecast-list-info-task))
  (-let (((&alist 'title title) org-taskforecast-list-info-task))
    (propertize title
                ;; TODO: define face
                'face 'org-scheduled-today)))

(defun org-taskforecast--create-task-list (today day-start)
  "Create a today's task list for TODAY.

This function returns a string as contents of `org-taskforecast-list-mode'.
Task list data are stored at each line of listed tasks.
To get them, use `org-taskforecast--list-get-task-link-at-point'.

- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (-as-> (org-taskforecast--get-task-links
          (org-taskforecast-get-dailylist-file today))
         links
         (let ((last-task-done-time
                (org-taskforecast--encode-hhmm day-start today))
               (org-taskforecast-list-info-today today)
               (org-taskforecast-list-info-now (current-time))
               (org-taskforecast-day-start day-start))
           (--map
            (-let* (((&alist 'original-id original-id) it)
                    (todo-type (org-taskforecast--get-task-link-todo-state-for-today
                                it today day-start))
                    (task (org-taskforecast--get-task-by-id original-id))
                    (org-taskforecast-list-info-task-start-end-time
                     (org-taskforecast--get-task-link-start-end-time
                      it
                      today
                      day-start
                      last-task-done-time
                      (and (eq todo-type 'todo)
                           org-taskforecast-list-info-now))))
              (prog1
                  (let ((org-taskforecast-list-info-task-link it)
                        (org-taskforecast-list-info-task task))
                    (-as-> org-taskforecast-list-task-formatters x
                           (-map #'funcall x)
                           (-reject #'s-blank-p x)
                           (s-join " " x)
                           (org-taskforecast--list-propertize-link-data x it)))
                ;; update last done time
                (-let (((&alist 'end end) org-taskforecast-list-info-task-start-end-time))
                  (setq last-task-done-time end))))
            links))
         (s-join "\n" links)))

(defun org-taskforecast--insert-task-list (today day-start)
  "Insert a TODAY's task list.

This function inserts contents of `org-taskforecast-list-mode'.

- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (insert (org-taskforecast--create-task-list today day-start)))

(defvar org-taskforecast-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-taskforecast-list-refresh)
    (define-key map (kbd "I") #'org-taskforecast-list-clock-in)
    (define-key map (kbd "O") #'org-taskforecast-list-clock-out)
    (define-key map (kbd "n") #'org-taskforecast-list-next-line)
    (define-key map (kbd "p") #'org-taskforecast-list-previous-line)
    (define-key map (kbd "t") #'org-taskforecast-list-link-todo)
    (define-key map (kbd "T") #'org-taskforecast-list-todo)
    (define-key map (kbd "e") #'org-taskforecast-list-set-effort)
    (define-key map (kbd "U") #'org-taskforecast-list-move-link-up)
    (define-key map (kbd "D") #'org-taskforecast-list-move-link-down)
    (define-key map (kbd "RET") #'org-taskforecast-list-goto-task)
    (define-key map (kbd "q") #'org-taskforecast-list-quit)
    map)
  "A key map for `org-taskforecast-list-mode'.")

(define-derived-mode org-taskforecast-list-mode nil "org-taskforecast list"
  "A major-mode to manage today's tasks."
  :group 'org-taskforecast
  (setq-local truncate-lines t))

(defvar org-taskforecast--list-buffer-name "*org-taskforecast list*"
  "A buffer name for `org-taskforecast-list-mode'.")

(defun org-taskforecast--get-list-buffer ()
  "Get the buffer for `org-taskforecast-list-mode'.

When the buffer is not found, this function returns nil."
  (get-buffer org-taskforecast--list-buffer-name))

(defun org-taskforecast--create-list-buffer (today day-start)
  "Create a buffer for `org-taskforecast-list-mode'.

If the buffer already exists, only returns the buffer.

- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((buffer (org-taskforecast--get-list-buffer)))
    (or buffer
        (with-current-buffer (get-buffer-create
                              org-taskforecast--list-buffer-name)
          (org-taskforecast-list-mode)
          (save-excursion
            (org-taskforecast--insert-task-list today day-start))
          (current-buffer)))))

;;;###autoload
(defun org-taskforecast-list ()
  "Show the buffer of `org-taskforecast-list-mode'."
  (interactive)
  (switch-to-buffer
   (org-taskforecast--create-list-buffer
    (org-taskforecast-today)
    org-taskforecast-day-start)))

(defun org-taskforecast--list-refresh ()
  "Refresh `org-taskforecast-list-mode' buffer."
  (-when-let (buffer (org-taskforecast--get-list-buffer))
    (with-current-buffer buffer
      (let ((current-link (org-taskforecast--list-get-task-link-at-point)))
        (erase-buffer)
        (org-taskforecast--insert-task-list
         (org-taskforecast-today)
         org-taskforecast-day-start)
        ;; Restore the line position of the cursor
        (goto-char (point-min))
        (-when-let* ((current-link current-link)
                     (pmatch (text-property-search-forward
                              org-taskforecast--list-task-link-property
                              current-link
                              (lambda (a b)
                                (-when-let* (((&alist 'id aid) a)
                                             ((&alist 'id bid) b))
                                  (string-equal aid bid))))))
          (goto-char (prop-match-beginning pmatch)))))))

(defun org-taskforecast--list-refresh-maybe ()
  "Refresh `org-taskforecast-list-mode' buffer if `org-taskforecast-auto-refresh-list-buffer' is non-nil."
  (when org-taskforecast-auto-refresh-list-buffer
    (org-taskforecast--list-refresh)))

(defun org-taskforecast-list-refresh ()
  "Refresh `org-taskforecast-list-mode' buffer."
  (interactive)
  (if (org-taskforecast--get-list-buffer)
      (org-taskforecast--list-refresh)
    (user-error "List buffer, %s, is not found"
                org-taskforecast--list-buffer-name)))

(defun org-taskforecast-list-clock-in ()
  "Start the clock on the task linked from the current line."
  (interactive)
  (-if-let ((&alist 'original-id original-id)
            (org-taskforecast--list-get-task-link-at-point))
      (progn
        (org-taskforecast--at-id original-id
          (org-clock-in))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-clock-out ()
  "Stop the current running clock."
  (interactive)
  (org-clock-out)
  (org-taskforecast--list-refresh))

(defun org-taskforecast-list-next-line ()
  "Go to the next line."
  (interactive)
  (let ((lastpos (point)))
    ;; Prevent moving cursor to the end of line when the current line is
    ;; the last line of the current buffer.
    ;; When that condition, `next-line' signals an error from `line-move'.
    (condition-case err
        (call-interactively #'next-line)
      ((end-of-buffer)
       (goto-char lastpos)
       (signal (car err) (cdr err))))))

(defun org-taskforecast-list-previous-line ()
  "Go to the previous line."
  (interactive)
  (call-interactively #'previous-line))

(defun org-taskforecast-list-goto-task ()
  "Go to the task linked from the current line."
  (interactive)
  (-if-let ((&alist 'original-id original-id)
            (org-taskforecast--list-get-task-link-at-point))
      (org-id-goto original-id)
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-todo ()
  "Change the TODO state of the task linked from the current line."
  (interactive)
  (-if-let ((&alist 'original-id original-id)
            (org-taskforecast--list-get-task-link-at-point))
      (progn
       (org-taskforecast--at-id original-id
         (org-todo))
       (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-link-todo ()
  "Change the TODO state of the task link at the current line."
  (interactive)
  (-if-let ((&alist 'id id)
            (org-taskforecast--list-get-task-link-at-point))
      (progn
        (org-taskforecast--at-id id
          (org-todo))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-set-effort ()
  "Change Effort property of the task at the current line."
  (interactive)
  (-if-let ((&alist 'original-id original-id)
            (org-taskforecast--list-get-task-link-at-point))
      (progn
        (org-taskforecast--at-id original-id
          (org-set-effort))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-move-link-up (&optional arg)
  "Move task link at the current line up past ARG others."
  (interactive "p")
  (-if-let ((&alist 'id id)
            (org-taskforecast--list-get-task-link-at-point))
      (progn
        (org-taskforecast--at-id id
          (org-move-subtree-up arg))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-move-link-down (&optional arg)
  "Move task link at the current line down past ARG others."
  (interactive "p")
  (org-taskforecast-list-move-link-up (- arg)))

(defun org-taskforecast-list-quit ()
  "Quit the today's task list buffer."
  (interactive)
  (quit-window))


;;;; org-taskforecast-track-mode

(defun org-taskforecast--track-register-task ()
  "Register clock-in task."
  (-let (((&alist 'todo-type todo-type) (org-taskforecast--get-task))
         (today (org-taskforecast-today)))
    ;; TODO: should consider a case that a done task is clocked?
    (when (eq todo-type 'todo)
      (org-taskforecast--push-task-link-maybe
       (org-id-get-create)
       (org-taskforecast-get-dailylist-file today)
       today
       org-taskforecast-day-start)
      ;; update list buffer
      (when (org-taskforecast--get-list-buffer)
        (org-taskforecast--list-refresh-maybe)))))

(defvar org-taskforecast-track-mode nil
  "Track changes of original tasks and update today's task list.")

(define-minor-mode org-taskforecast-track-mode
  "Track clocking original tasks and update today's task list."
  :group 'org-taskforecast
  :global nil
  (if org-taskforecast-track-mode
      (progn
        (add-hook 'org-clock-in-hook
                  #'org-taskforecast--track-register-task
                  nil
                  t))
    (remove-hook 'org-clock-in-hook
                 #'org-taskforecast--track-register-task
                 t)))

(provide 'org-taskforecast)
;;; org-taskforecast.el ends here

;; Local Variables:
;; eval: (when (fboundp 'flycheck-mode) (flycheck-mode 1))
;; eval: (when (fboundp 'flycheck-package-setup) (flycheck-package-setup))
;; byte-compile-error-on-warn: t
;; End:
