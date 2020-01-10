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
- `org-taskforecast-list-info-task-link' as an instance of
  `org-taskforecast--tlink'
- `org-taskforecast-list-info-task' as an instance of
  `org-taskforecast--task'
- `org-taskforecast-list-info-today' as an encoded time
- `org-taskforecast-list-info-now' as an encoded time
- `org-taskforecast-list-info-task-start-end-time' as an instance of
  `org-taskforecast--tlclock'

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

;;; Type

(defun org-taskforecast--encoded-time-p (x)
  "Non-nil means X is an encoded time.

Encoded time is a type of a returned value of `encode-time'."
  (or
   ;; `time-add' possibly returns an integer
   (integerp x)
   (and (listp x)
        (member (length x) '(2 3 4))
        (-all-p #'integerp x))))

(cl-deftype org-taskforecast--encoded-time ()
  '(satisfies org-taskforecast--encoded-time-p))

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

(defun org-taskforecast--time-to-hhmm (time today)
  "Convert TIME to hour and minute as time of TODAY.

A returned value is a list like (hour minute)."
  (let* ((today (org-taskforecast--time-as-date today))
         (dsec (floor (time-to-seconds (time-subtract time today))))
         (dmin (/ dsec 60))
         (hour (/ dmin 60))
         (minute (% dmin 60)))
    (org-taskforecast-assert (<= 0 dsec))
    (list hour minute)))

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
    :type org-taskforecast--encoded-time
    :documentation
    "A start time of a clock of a task as an encoded time.")
   (end
    :initarg :end
    :reader org-taskforecast--clock-end
    :type (or null org-taskforecast--encoded-time)
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


(defclass org-taskforecast--scheduled ()
  ((start-time
    :initarg :start-time
    :reader org-taskforecast--scheduled-start-time
    :type org-taskforecast--encoded-time
    :documentation
    "An encoded time of the start time of a schedule.")
   (date-only-p
    :initarg :date-only-p
    :reader org-taskforecast--scheduled-date-only-p
    :type boolean
    :documentation
    "Non-nil means the time stamp of a schedule has no hour and minute sections.")
   (repeatp
    :initarg :repeatp
    :reader org-taskforecast--scheduled-repeat-p
    :type boolean
    :documentation
    "Non-nil means the time stamp of a schedule has a repeater."))
  :documentation
  "A SCHEDULED property of a task.")

(defun org-taskforecast--get-scheduled-from-timestamp (timestamp)
  "Get a scheduled information from TIMESTAMP.

TIMESTAMP is a timestamp element of a scheduled property of a heading
of org element api.
This function returns an instance of `org-taskforecast--scheduled'."
  (let ((start-time (org-taskforecast--timestamp-start-time timestamp))
        (date-only-p (not (or (org-element-property :hour-start timestamp)
                              (org-element-property :minute-start timestamp))))
        (repeatp (and (org-element-property :repeater-type timestamp) t)))
    (org-taskforecast--scheduled
     :start-time start-time
     :date-only-p date-only-p
     :repeatp repeatp)))

(defclass org-taskforecast--deadline ()
  ((time
    :initarg :time
    :reader org-taskforecast--deadline-time
    :type org-taskforecast--encoded-time
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
  "A DEADLINE property of a task.")

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

(defclass org-taskforecast--task ()
  ((id
    :initarg :id
    :reader org-taskforecast--task-id
    :type string
    :documentation
    "An ID of org-id.")
   (title
    :initarg :title
    :reader org-taskforecast--task-title
    :type string
    :documentation
    "A heading title.")
   (effort
    :initarg :effort
    :reader org-taskforecast--task-effort
    :type (or null string)
    :documentation
    "A value of Effort property.")
   (state
    :initarg :state
    :reader org-taskforecast--task-state
    :type symbol
    :documentation
    "A todo state as a symbol of todo, running or done.")
   (clocks
    :initarg :clocks
    :reader org-taskforecast--task-clocks
    :type list
    :documentation
    "A list of clock data, each element is an instance of `org-taskforecast--clock'.")
   (todo
    :initarg :todo
    :reader org-taskforecast--task-todo
    :type (or null string)
    :documentation
    "A todo state string.")
   (todo-type
    :initarg :todo-type
    :reader org-taskforecast--task-todo-type
    :type (or null symbol)
    :documentation
    "A type of todo as a symbol of todo or done.")
   (scheduled
    :initarg :scheduled
    :reader org-taskforecast--task-scheduled
    :type (or null org-taskforecast--scheduled)
    :documentation
    "A schedule infomaton.")
   (deadline
    :initarg :deadline
    :reader org-taskforecast--task-deadline
    :type (or null org-taskforecast--deadline)
    :documentation
    "A deadline information."))
  :documentation
  "A task heading data.")

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
  "Get a task at the current point.

A returned value is an instance of `org-taskforecast--task'."
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
           (state (cond
                   ((and (eq todo-type 'todo) running-p) 'running)
                   ((and (eq todo-type 'todo) (not running-p)) 'todo)
                   ((eq todo-type 'done) 'done)
                   (t (error "Not a task heading")))))
      (org-taskforecast--task
       :id id
       :title title
       :effort effort
       :state state
       :clocks clocks
       :todo todo
       :todo-type todo-type
       :scheduled scheduled
       :deadline deadline))))

(defun org-taskforecast--get-task-by-id (id)
  "Get a task by ID.

A returned value is an instance of `org-taskforecast--task'."
  (org-taskforecast--at-id id
    (org-taskforecast--get-task)))

(defclass org-taskforecast--tlink ()
  ((id
    :initarg :id
    :reader org-taskforecast--tlink-id
    :type string
    :documentation
    "An ID of org-id.")
   (task-id
    :initarg :task-id
    :reader org-taskforecast--tlink-task-id
    :type string
    :documentation
    "An ID of a task where this links to.")
   (effective-start-time
    :initarg :effective-start-time
    :reader org-taskforecast--tlink-effective-start-time
    :type (or null org-taskforecast--encoded-time)
    :documentation
    "An encoded time when the task link is effective after.")
   (effective-end-time
    :initarg :effective-end-time
    :reader org-taskforecast--tlink-effective-end-time
    :type (or null org-taskforecast--encoded-time)
    :documentation
    "An encoded time when the task link is effective before."))
  :documentation
  "A task link data.")

(defun org-taskforecast--get-link-id (str)
  "Get a link id from STR.

STR is a org-id link string like \"[[id:1234][foo]]\".
If STR is not a org-id link string, this function returns nil."
  (let ((re (rx bos "[[id:" (group (+ (not (any "]")))) "]["
                (+ (not (any "]"))) "]]" eos)))
    (-when-let (((_ id)) (s-match-strings-all re str))
      id)))

(defun org-taskforecast--task-todo-state-for-today (task date day-start)
  "Get todo state of TASK for today.

This function returns a symbol, todo or done.
- TASK is an instance of `org-taskforecast--task'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let* ((todo-type (org-taskforecast--task-todo-type task))
         (scheduled (org-taskforecast--task-scheduled task))
         (deadline (org-taskforecast--task-deadline task))
         (stime (when scheduled
                  (let ((time (org-taskforecast--scheduled-start-time scheduled)))
                    (if (org-taskforecast--scheduled-date-only-p scheduled)
                        (org-taskforecast--encode-hhmm day-start time)
                      time))))
         (dtime (when deadline
                  (let ((time (org-taskforecast--deadline-time deadline)))
                    (if (org-taskforecast--deadline-date-only-p deadline)
                        (org-taskforecast--encode-hhmm day-start time)
                      time))))
         (repeatp (or (and scheduled (org-taskforecast--scheduled-repeat-p scheduled))
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

(defun org-taskforecast--tlink-todo-state-for-today (task-link date day-start)
  "Get todo state of TASK-LINK for today.

This function returns a symbol, todo or done.
- TASK-LINK is an instance of `org-taskforecast--tlink'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let ((task
         (org-taskforecast--get-task-by-id
          (org-taskforecast--tlink-task-id task-link)))
        (effective-end-time
         (org-taskforecast--tlink-effective-end-time task-link)))
    (if effective-end-time
        ;; interrupted if efective-end-time exists
        'done
      (org-taskforecast--task-todo-state-for-today task date day-start))))

(defun org-taskforecast--get-task-link ()
  "Get a task link at the current point.

A returned value is an instance of `org-taskforecast--tlink'.
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
      (-when-let* ((task-id (org-taskforecast--get-link-id title))
                   ;; Create id when this heading is a task link.
                   (id (org-id-get-create)))
        (org-taskforecast--tlink
         :id id
         :task-id task-id
         :effective-start-time effective-start-time
         :effective-end-time effective-end-time)))))

(defun org-taskforecast--get-task-link-by-id (id)
  "Get a task link by ID.

A returned value is an instance of `org-taskforecast--tlink'."
  (org-taskforecast--at-id id
    (org-taskforecast--get-task-link)))

(defun org-taskforecast--append-task-link (id file)
  "Append a task link for ID to the end of FILE.

This function returns an ID of the new task link."
  (let ((normalized-title
         (org-taskforecast--normalize-title
          (org-taskforecast--task-title
           (org-taskforecast--get-task-by-id id)))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (concat "* [[id:" id "][" normalized-title "]]\n"))
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
            (org-taskforecast--tlink-todo-state-for-today
             it date day-start))
        it)
       (-if-let* ((task-link (-first-item it))
                  (link-id (org-taskforecast--tlink-id task-link)))
           link-id
         (org-taskforecast--append-task-link id file))))

(defun org-taskforecast--get-first-todo-task-link (file date day-start)
  "Get the first todo task link in FILE.

A returned value is an instance of `org-taskforecast--tlink'.
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
                       (org-taskforecast--tlink-todo-state-for-today
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

(defun org-taskforecast--tlink-has-effective-clock (task-link date day-start)
  "Non-nil means TASK-LINK has some effective clocks.

An effective clock is a clock information that clocked today.
If the task link has effective start/end time, an effective clock satisfies
the following conditions:
- the clock was started after the effective start time
- the clock was ended before the effective end time

- TASK-LINK is an instance of `org-taskforecast--tlink'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let* ((task-id
          (org-taskforecast--tlink-task-id task-link))
         (effective-start-time
          (org-taskforecast--tlink-effective-start-time task-link))
         (effective-end-time
          (org-taskforecast--tlink-effective-end-time task-link))
         (clocks
          (org-taskforecast--task-clocks
           (org-taskforecast--get-task-by-id task-id))))
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

(defun org-taskforecast--tlink-effective-effort (task-link date day-start)
  "Get effort value of TASK-LINK.

A returned value is an effort second.

- TASK-LINK is an instance of `org-taskforecast--tlink'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let* ((effective-start-time
          (org-taskforecast--tlink-effective-start-time task-link))
         (effective-end-time
          (org-taskforecast--tlink-effective-end-time task-link))
         (task
          (org-taskforecast--get-task-by-id
           (org-taskforecast--tlink-task-id task-link)))
         (clocks
          (org-taskforecast--task-clocks task))
         (effort-sec
          (org-taskforecast--effort-to-second
           (org-taskforecast--task-effort task)))
         (time-greater-p
          (-flip #'time-less-p))
         (today-start
          (org-taskforecast--encode-hhmm day-start date))
         (range-start
          (-max-by time-greater-p
                   (-non-nil (list today-start effective-start-time))))
         (range-end
          (-min-by time-greater-p
                   (-non-nil
                    (list effective-end-time
                          (org-taskforecast--encode-hhmm (+ day-start 2400)
                                                         date)))))
         (dsec
          (-compose #'time-to-seconds #'org-taskforecast--clock-duration))
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
  (let* ((task-id (org-taskforecast--tlink-task-id
                   (org-taskforecast--get-task-link-by-id link-id)))
         (new-link-id (org-taskforecast--append-task-link task-id file))
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
                 (task-id
                  (org-taskforecast--tlink-task-id first-todo-task-link))
                 (link-id
                  (org-taskforecast--tlink-id first-todo-task-link)))
      (when (and (not (equal id task-id))
                 (org-taskforecast--tlink-has-effective-clock
                  first-todo-task-link
                  date
                  day-start))
        (org-taskforecast--split-task-link link-id (current-time) file))))
  ;; TODO: consider a case that the task of ID is already done
  (let* ((link-id (org-taskforecast--append-task-link-maybe
                   id file date day-start))
         (task-link (org-taskforecast--get-task-link-by-id link-id))
         (todo-type (org-taskforecast--tlink-todo-state-for-today
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

(defclass org-taskforecast--tlclock ()
  ((start
    :initarg :start
    :reader org-taskforecast--tlclock-start
    :type org-taskforecast--encoded-time
    :documentation
    "an encoded time that indicates the start time of the task of today.
If the start time is not found, the value will be an estimated time.")
   (end
    :initarg :end
    :reader org-taskforecast--tlclock-end
    :type org-taskforecast--encoded-time
    :documentation
    "An encoded time that indicates the end time of the task of today.
If the end time is not found, the value will be an estimated time.")
   (start-estimated-p
    :initarg :start-estimated-p
    :reader org-taskforecast--tlclock-start-estimated-p
    :type boolean
    :documentation
    "Non-nil means the start time is estimated.")
   (end-estimated-p
    :initarg :end-estimated-p
    :reader org-taskforecast--tlclock-end-estimated-p
    :type boolean
    :documentation
    "Non-nil means the end time is estimated.")
   (overrunp
    :initarg :overrunp
    :reader org-taskforecast--tlclock-overrun-p
    :type boolean
    :documentation
    "Non-nil means the end time is over the time of the start time plus effort."))
  :documentation
  "An information of start and end time of a task link.")

(defun org-taskforecast--tlink-start-end-time (task-link date day-start &optional start-after now)
  "Get the start and end time of a TASK-LINK.

This function returns an instance of `org-taskforecast--tlclock'.

- TASK-LINK is an instance of `org-taskforecast--tlink'
- DATE is an encoded time as the date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- START-AFTER is an encoded time (optional).
  If it is set, ignore clocks whose start time is earlier than it.
- NOW is an encoded time (optional).
  If it is set, use it instead of an estimated time of start or end
  when the estimated time is earlier than it."
  (let* ((day-start-time
          (org-taskforecast--encode-hhmm day-start date))
         (next-day-start-time
          (org-taskforecast--encode-hhmm (+ day-start 2400) date))
         (effective-start-time
          (org-taskforecast--tlink-effective-start-time task-link))
         (effective-end-time
          (org-taskforecast--tlink-effective-end-time task-link))
         (todo
          (org-taskforecast--tlink-todo-state-for-today
           task-link date day-start))
         (task
          (org-taskforecast--get-task-by-id
           (org-taskforecast--tlink-task-id task-link)))
         (effort-sec
          (or (org-taskforecast--tlink-effective-effort task-link date day-start)
              0))
         (clock-start-greater-p
          (-flip #'org-taskforecast--clock-start-less-p))
         (time-greater-p
          (-flip #'time-less-p))
         (start-after
          (-max-by time-greater-p
                   (-non-nil
                    (list start-after day-start-time effective-start-time))))
         (end-before
          (-min-by time-greater-p
                   (-non-nil
                    (list next-day-start-time effective-end-time))))
         (target-clocks
          (org-taskforecast--get-clocks-in-range
           (org-taskforecast--task-clocks task)
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
         (start-estimated-p
          (null start-time))
         (end-estimated-p
          (or (null end-time) (eq todo 'todo)))
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
         (overrunp
          (time-less-p start-plus-effort end)))
    (org-taskforecast--tlclock
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
   (string= task-id (org-taskforecast--tlink-task-id it))
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
                (org-taskforecast--tlink-todo-state-for-today
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

This value will be an instance of `org-taskforecast--tlink'.
See `org-taskforecast-list-task-formatters' for more detail.")

(defvar org-taskforecast-list-info-task nil
  "This variable is used to pass a task data to formatters.

This value will be an instance of `org-taskforecast--task'.
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

This value will be an instance of `org-taskforecast--tlclock'.
See `org-taskforecast-list-task-formatters' for more detail.")

(defun org-taskforecast-list-format-effort ()
  "Format effort property of a task.

This function is used for `org-taskforecast-list-task-formatters'."
  (let ((effort (org-taskforecast--tlink-effective-effort
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
   (let ((decoded (decode-time org-taskforecast-list-info-today)))
     (and (= (decoded-time-hour decoded)
             (decoded-time-minute decoded)
             (decoded-time-second decoded)
             0))))
  (-let* ((start
           (org-taskforecast--tlclock-start
            org-taskforecast-list-info-task-start-end-time))
          (start-estimated-p
           (org-taskforecast--tlclock-start-estimated-p
            org-taskforecast-list-info-task-start-end-time))
          ((hour minute)
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
   (let ((decoded (decode-time org-taskforecast-list-info-today)))
     (and (= (decoded-time-hour decoded)
             (decoded-time-minute decoded)
             (decoded-time-second decoded)
             0))))
  (-let* ((todo-type
           (org-taskforecast--tlink-todo-state-for-today
            org-taskforecast-list-info-task-link
            org-taskforecast-list-info-today
            org-taskforecast-day-start))
          (end
           (org-taskforecast--tlclock-end
            org-taskforecast-list-info-task-start-end-time))
          (end-estimated-p
           (org-taskforecast--tlclock-end-estimated-p
            org-taskforecast-list-info-task-start-end-time))
          (overrunp_
           (org-taskforecast--tlclock-overrun-p
            org-taskforecast-list-info-task-start-end-time))
          (overrunp
           (and end-estimated-p
                (eq todo-type 'todo)
                overrunp_))
          ((hour minute)
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
  (let ((todo-type
         (org-taskforecast--tlink-todo-state-for-today
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
  (let ((title (org-taskforecast--task-title org-taskforecast-list-info-task)))
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
            (let* ((todo-type
                    (org-taskforecast--tlink-todo-state-for-today
                     it today day-start))
                   (task
                    (org-taskforecast--get-task-by-id
                     (org-taskforecast--tlink-task-id it)))
                   (org-taskforecast-list-info-task-start-end-time
                    (org-taskforecast--tlink-start-end-time
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
                (setq last-task-done-time
                      (org-taskforecast--tlclock-end
                       org-taskforecast-list-info-task-start-end-time))))
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
    (define-key map (kbd "t") #'org-taskforecast-list-todo)
    (define-key map (kbd "e") #'org-taskforecast-list-set-effort)
    (define-key map (kbd "U") #'org-taskforecast-list-move-link-up)
    (define-key map (kbd "D") #'org-taskforecast-list-move-link-down)
    (define-key map (kbd "d") #'org-taskforecast-list-remove-link)
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
                                (when (and a b)
                                  (string-equal
                                   (org-taskforecast--tlink-id a)
                                   (org-taskforecast--tlink-id b)))))))
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
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast--tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
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
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast--tlink-task-id task-link)))
      (org-id-goto task-id)
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-todo ()
  "Change the TODO state of the task linked from the current line."
  (interactive)
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast--tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-todo))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-set-effort ()
  "Change Effort property of the task at the current line."
  (interactive)
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast--tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-set-effort))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-move-link-up (&optional arg)
  "Move task link at the current line up past ARG others."
  (interactive "p")
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (id (org-taskforecast--tlink-id task-link)))
      (progn
        (org-taskforecast--at-id id
          (org-move-subtree-up arg))
        (org-taskforecast--list-refresh))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-move-link-down (&optional arg)
  "Move task link at the current line down past ARG others."
  (interactive "p")
  (org-taskforecast-list-move-link-up (- arg)))

(defun org-taskforecast-list-remove-link ()
  "Remove a task link at the current line."
  (interactive)
  (-when-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
               (id (org-taskforecast--tlink-id task-link))
               (title (org-taskforecast--task-title
                       (org-taskforecast--get-task-by-id
                        (org-taskforecast--tlink-task-id task-link)))))
    (org-taskforecast--at-id id
      (save-restriction
        (save-excursion
          (widen)
          (org-narrow-to-subtree)
          (delete-region (point-min) (point-max)))))
    ;; Move the cursor to the next line or the previous line to prevent
    ;; moving the cursor to the top of a task list.
    (when (or (/= 0 (forward-line 1)) (eobp))
      (forward-line -1))
    (org-taskforecast--list-refresh)
    (message "%s has been removed from task list." title)))

(defun org-taskforecast-list-quit ()
  "Quit the today's task list buffer."
  (interactive)
  (quit-window))


;;;; org-taskforecast-track-mode

(defun org-taskforecast--track-register-task ()
  "Register clock-in task."
  (let ((todo-type (org-taskforecast--task-todo-type
                    (org-taskforecast--get-task)))
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
