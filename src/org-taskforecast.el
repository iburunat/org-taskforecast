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
(require 'text-property-search)
(require 'org)
(require 'org-element)
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
- `org-taskforecast-list-info-last-task-done-time' as an encoded time
- `org-taskforecast-list-info-today' as an encoded time
- `org-taskforecast-list-info-now' as an encoded time

Other global variables also are set for formatting:
- `org-taskforecast-day-start'"
  :type '(repeat function)
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

(defun org-taskforecast-get-dailylist-file (today &optional create)
  "Get the path of today's daily task list file for TODAY.

When CREATE is set, this function creates the file and its directory.

This function depends on:
- `org-taskforecast-dailylist-file' as a file format
- `org-taskforecast-day-start' to determine the date of today"
  (let* ((file (expand-file-name
                (format-time-string org-taskforecast-dailylist-file today))))
    (when (and create (not (file-exists-p file)))
      (make-directory (file-name-directory file) t)
      (write-region "" nil file))
    file))

;;; Org-mode

(defmacro org-taskforecast--at-id (id &rest body)
  "Eval BODY at a heading of ID."
  (declare (indent 1))
  `(-let (((file . pos) (org-id-find ,id)))
     (with-current-buffer (find-file-noselect file)
       (save-excursion
         (goto-char pos)
         ,@body))))

(defun org-taskforecast--normalize-title (title)
  "Normalize a TITLE of a heading."
  (s-replace-all '(("[" . "{") ("]" . "}"))
                 (org-link-display-format title)))

(defun org-taskforecast--parse-heading ()
  "Parse heading at point by org element api."
  (save-excursion
    (save-restriction
      (widen)
      (org-narrow-to-subtree)
      (org-element-parse-buffer))))

(org-taskforecast-defalist org-taskforecast--clock-alist
    (start end)
  "A clock data.

- START is the start time as an encoded time
- END is the end time as an encoded time (optional)")

(defun org-taskforecast--clock-start-less-p (a b)
  "Compare start-times of A and B by `time-less-p'.

A and B are `org-taskforecast--clock-alist's."
  (-let (((&alist 'start as) a)
         ((&alist 'start bs) b))
    (time-less-p as bs)))

(org-taskforecast-defalist org-taskforecast--task-alist
    (id title effort status clocks)
  "Alist of a task.

The task is a heading linked from daily task list file.
- ID is an id of org-id
- TITLE is a heading title
- EFFORT is a value of effort property
- STATUS is a symbol of todo, running and done
- CLOCKS is a list of clock data, each element is an alist of
  `org-taskforecast--clock-alist'")

(defun org-taskforecast--get-clock-from-element (element)
  "Get a clock from ELEMENT.

ELEMENT is a clock element of org element api."
  (let* ((timestamp (org-element-property :value element))
         (runnigp (eq 'running (org-element-property :status element)))
         (start (encode-time
                 0
                 (org-element-property :minute-start timestamp)
                 (org-element-property :hour-start timestamp)
                 (org-element-property :day-start timestamp)
                 (org-element-property :month-start timestamp)
                 (org-element-property :year-start timestamp)))
         (end (and (not runnigp)
                   (encode-time
                    0
                    (org-element-property :minute-end timestamp)
                    (org-element-property :hour-end timestamp)
                    (org-element-property :day-end timestamp)
                    (org-element-property :month-end timestamp)
                    (org-element-property :year-end timestamp)))))
    (org-taskforecast--clock-alist :start start :end end)))

(defun org-taskforecast--get-task ()
  "Get a task as an alist.

A returned value is an alist of `org-taskforecast--task-alist'."
  (let* ((id (org-id-get-create))
         (element (org-element-at-point))
         (title (org-taskforecast--normalize-title
                 (substring-no-properties (org-element-property :title element))))
         (effort (org-entry-get nil org-effort-property))
         (todo-type (org-element-property :todo-type element))
         (helement (org-taskforecast--parse-heading))
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
     :clocks clocks)))

(defun org-taskforecast--get-task-by-id (id)
  "Get a task alist by ID.

A returned value is an alist of `org-taskforecast--task-alist'."
  (org-taskforecast--at-id id
    (org-taskforecast--get-task)))

(org-taskforecast-defalist org-taskforecast--task-link-alist
    (id original-id todo todo-type)
  "Alist of a task link.

It links to a task heading.
- ID is an id of org-id
- ORIGINAL-ID is where this links to
- TODO is a string of a todo state (optional)
- TODO-TYPE is a symbol of a type of todo (optional)")

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
  (let* ((element (org-element-at-point))
         (title (org-element-property :title element))
         (todo (org-element-property :todo-keyword element))
         (todo-type (org-element-property :todo-type element)))
    (-when-let* ((original-id (org-taskforecast--get-link-id title))
                 ;; Create id when this heading is a task link.
                 (id (org-id-get-create)))
      (org-taskforecast--task-link-alist :id id
                                         :original-id original-id
                                         :todo todo
                                         :todo-type todo-type))))

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

(org-taskforecast-defalist org-taskforecast--task-satrt-end-time-alist
    (start end start-estimated-p end-estimated-p)
  "An information of start and end time of a task.

- START is an encoded time that indicates the start time of the task of today.
  If the start time is not found, the value will be an estimated time.
- END is an encoded time that indicates the end time of the task of today.
  If the end time is not found, the value will be an estimated time.
- START-ESTIMATED-P is a boolean.
  If the start time is estimated, its value is non-nil.
- END-ESTIMATED-P is a boolean.
  If the end time is estimated, its value is non-nil.")

(defun org-taskforecast--get-task-start-end-time (task date day-start
                                                       &optional start-after)
  "Get the start and end time of a TASK.

This function returns a `org-taskforecast--task-satrt-end-time-alist'.

- TASK is an alist of `org-taskforecast--task-alist'
- DATE is an encoded time as the date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- START-AFTER is an encoded time.
  If it is set, ignore clocks whose start time is earlier than it."
  (org-taskforecast-assert (org-taskforecast--task-alist-type-p task))
  (-let* ((day-start-time (org-taskforecast--encode-hhmm day-start date))
          (start-after (or start-after day-start-time))
          ((&alist 'effort effort 'clocks clocks 'status status) task)
          (target-clocks
           (--filter
            (-let (((&alist 'start start) it))
              (and (org-taskforecast--today-p start date day-start)
                   (not (time-less-p start start-after))))
            clocks))
          (start-time
           (-let (((&alist 'start)
                   (when target-clocks
                     (--min-by (not (org-taskforecast--clock-start-less-p
                                     it other))
                               target-clocks))))
             start))
          (end-time
           (-let (((&alist 'end)
                   (when target-clocks
                     (--max-by (not (org-taskforecast--clock-start-less-p
                                     it other))
                               target-clocks))))
             end))
          (start-estimated-p (null start-time))
          (end-estimated-p (or (null end-time) (memq status '(todo running))))
          (start (if start-estimated-p start-after start-time))
          (end (if end-estimated-p
                   (time-add start
                             (seconds-to-time
                              (or (org-taskforecast--effort-to-second effort)
                                  0)))
                 end-time)))
    (org-taskforecast--task-satrt-end-time-alist
     :start start
     :end end
     :start-estimated-p start-estimated-p
     :end-estimated-p end-estimated-p)))

(defun org-taskforecast--get-task-links-for-task (task-id file)
  "Get task links for the task of TASK-ID in FILE.

- TASK-ID is a string
- FILE is a today's daily task list file name"
  (--filter
   (-let (((&alist 'original-id original-id) it))
     (string= task-id original-id))
   (org-taskforecast--get-task-links file)))

(defun org-taskforecast--get-todo-link-head-pos (file)
  "Get the point of head of todo task links in FILE."
  (let ((pos nil))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (org-map-entries
         (lambda ()
           (unless pos
             (--> (org-element-at-point)
                  (org-element-property :todo-type it)
                  (when (eq it 'todo)
                    (setq pos (point))))))
         nil
         'file)
        (or pos (point-max))))))

(defun org-taskforecast--cut-heading-by-id (id)
  "Cut a heading by ID.

Return a string of the heading.
When this function failed, returns nil."
  (org-taskforecast--at-id id
    (-when-let* ((helement (org-element-at-point))
                 (begin (org-element-property :begin helement))
                 (end (org-element-property :end helement)))
      (prog1
          (buffer-substring begin end)
        (delete-region begin end)))))


;;;; General Commands

;;; Registration

;;;###autoload
(defun org-taskforecast-register-task ()
  "Register a task at point as a task for today."
  (interactive)
  (org-taskforecast--append-task-link
   (org-id-get-create)
   (org-taskforecast-get-dailylist-file (org-taskforecast-today) t)
   org-taskforecast-default-todo)
  (org-taskforecast--list-refresh))


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

(defvar org-taskforecast-list-info-last-task-done-time nil
  "This variable is used to pass a last task done time to formatters.

This value will be an encoded time.
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

(defun org-taskforecast-list-format-effort ()
  "Format effort property of a task.

This function is used for `org-taskforecast-list-task-formatters'."
  (org-taskforecast-assert
   (org-taskforecast--task-alist-type-p org-taskforecast-list-info-task))
  (-let (((&alist 'effort effort) org-taskforecast-list-info-task))
    (format "%5s" (or effort "-:--"))))

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
           (org-taskforecast--get-task-start-end-time
            org-taskforecast-list-info-task
            org-taskforecast-list-info-today
            org-taskforecast-day-start
            org-taskforecast-list-info-last-task-done-time))
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
  (-let* (((&alist 'todo-type todo-type)
           org-taskforecast-list-info-task-link)
          ((&alist 'end end 'end-estimated-p end-estimated-p)
           (org-taskforecast--get-task-start-end-time
            org-taskforecast-list-info-task
            org-taskforecast-list-info-today
            org-taskforecast-day-start
            org-taskforecast-list-info-last-task-done-time))
          (overrunp (and end-estimated-p
                         (eq todo-type 'todo)
                         (time-less-p end org-taskforecast-list-info-now)))
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
  (-when-let ((&alist 'todo todo 'todo-type todo-type)
              org-taskforecast-list-info-task-link)
    (propertize todo
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
         (let ((org-taskforecast-list-info-last-task-done-time
                (org-taskforecast--encode-hhmm day-start today))
               (org-taskforecast-list-info-today today)
               (org-taskforecast-list-info-now (current-time))
               (org-taskforecast-day-start day-start))
           (--map
            (-let* (((&alist 'original-id original-id 'todo-type todo-type) it)
                    (task (org-taskforecast--get-task-by-id original-id)))
              (prog1
                  (let ((org-taskforecast-list-info-task-link it)
                        (org-taskforecast-list-info-task task))
                    (-as-> org-taskforecast-list-task-formatters x
                           (-map #'funcall x)
                           (-reject #'s-blank-p x)
                           (s-join " " x)
                           (org-taskforecast--list-propertize-link-data x it)))
                ;; update last done time
                (-let* (((&alist 'end end 'end-estimated-p end-estimated-p)
                         (org-taskforecast--get-task-start-end-time
                          task
                          today
                          day-start
                          org-taskforecast-list-info-last-task-done-time))
                        (overrunp
                         (and end-estimated-p
                              (eq todo-type 'todo)
                              (time-less-p end
                                           org-taskforecast-list-info-now))))
                  (setq org-taskforecast-list-info-last-task-done-time
                        (if overrunp org-taskforecast-list-info-now end)))))
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
  :group 'org-taskforecast)

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



(provide 'org-taskforecast)
;;; org-taskforecast.el ends here
