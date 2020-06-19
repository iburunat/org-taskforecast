;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'dash)
(require 'org-taskforecast)
(require 'org-taskforecast-test-helper)

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


;;;; task-forecast-list mode

;;;;; Major mode

(org-taskforecast-test-deftest org-taskforecast-list-move-link-to-section_without-sort ()
  "Move task link A to section 0900 without sorting"
  :agenda-file (_ "\
* TODO A
:PROPERTIES:
:ID:       id_task_aaaa
:END:

* TODO B
:PROPERTIES:
:ID:       id_task_bbbb
:END:

* TODO C
:PROPERTIES:
:ID:       id_task_cccc
:END:
")
  :dailylist-file (dailylist-file "\
* SECTION: 7:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0700
:ORG_TASKFORECAST_SECTION_START_TIME: 700
:ID:       id_section_0700
:END:
* [[id:id_task_aaaa][A]]
:PROPERTIES:
:ID:       id_tlink_aaaa
:END:
* [[id:id_task_bbbb][B]]
:PROPERTIES:
:ID:       id_tlink_bbbb
:END:
* SECTION: 9:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0900
:ORG_TASKFORECAST_SECTION_START_TIME: 900
:ID:       id_section_0900
:END:
* [[id:id_task_cccc][C]]
:PROPERTIES:
:ID:       id_tlink_cccc
:END:
")
  (let* ((now (current-time))
         (day-start org-taskforecast-day-start)
         (entries (org-taskforecast--get-entries dailylist-file day-start)))
    (org-taskforecast-list-move-link-to-section
     (--find (equal (org-taskforecast-entry-id it) "id_tlink_aaaa") entries)
     (--find (equal (org-taskforecast-entry-id it) "id_section_0900") entries)
     dailylist-file
     nil
     (org-taskforecast--date-of-time now day-start)
     day-start
     now))
  (should (equal
           (org-taskforecast-test-file-string-no-properties dailylist-file)
           "\
* SECTION: 7:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0700
:ORG_TASKFORECAST_SECTION_START_TIME: 700
:ID:       id_section_0700
:END:
* [[id:id_task_bbbb][B]]
:PROPERTIES:
:ID:       id_tlink_bbbb
:END:
* SECTION: 9:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0900
:ORG_TASKFORECAST_SECTION_START_TIME: 900
:ID:       id_section_0900
:END:
* [[id:id_task_cccc][C]]
:PROPERTIES:
:ID:       id_tlink_cccc
:END:
* [[id:id_task_aaaa][A]]
:PROPERTIES:
:ID:       id_tlink_aaaa
:END:
")))

(org-taskforecast-test-deftest org-taskforecast-list-move-link-to-section_with-sort ()
  "Move task link A to section 0900 with sorting"
  :agenda-file (_ "\
* TODO A
SCHEDULED: <2020-01-01 Wed>
:PROPERTIES:
:ID:       id_task_aaaa
:END:

* TODO B
:PROPERTIES:
:ID:       id_task_bbbb
:END:

* TODO C
:PROPERTIES:
:ID:       id_task_cccc
:END:
")
  :dailylist-file (dailylist-file "\
* SECTION: 7:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0700
:ORG_TASKFORECAST_SECTION_START_TIME: 700
:ID:       id_section_0700
:END:
* [[id:id_task_aaaa][A]]
:PROPERTIES:
:ID:       id_tlink_aaaa
:END:
* [[id:id_task_bbbb][B]]
:PROPERTIES:
:ID:       id_tlink_bbbb
:END:
* SECTION: 9:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0900
:ORG_TASKFORECAST_SECTION_START_TIME: 900
:ID:       id_section_0900
:END:
* [[id:id_task_cccc][C]]
:PROPERTIES:
:ID:       id_tlink_cccc
:END:
")
  (let* ((now (current-time))
         (day-start org-taskforecast-day-start)
         (entries (org-taskforecast--get-entries dailylist-file day-start)))
    (org-taskforecast-list-move-link-to-section
     (--find (equal (org-taskforecast-entry-id it) "id_tlink_aaaa") entries)
     (--find (equal (org-taskforecast-entry-id it) "id_section_0900") entries)
     dailylist-file
     ;; sort by SCHEDULED
     (list #'org-taskforecast-ss-time-up)
     (org-taskforecast--date-of-time now day-start)
     day-start
     now))
  (should (equal
           (org-taskforecast-test-file-string-no-properties dailylist-file)
           "\
* SECTION: 7:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0700
:ORG_TASKFORECAST_SECTION_START_TIME: 700
:ID:       id_section_0700
:END:
* [[id:id_task_bbbb][B]]
:PROPERTIES:
:ID:       id_tlink_bbbb
:END:
* SECTION: 9:00 -
:PROPERTIES:
:ORG_TASKFORECAST_SECTION_ID: 0900
:ORG_TASKFORECAST_SECTION_START_TIME: 900
:ID:       id_section_0900
:END:
* [[id:id_task_aaaa][A]]
:PROPERTIES:
:ID:       id_tlink_aaaa
:END:
* [[id:id_task_cccc][C]]
:PROPERTIES:
:ID:       id_tlink_cccc
:END:
")))

;;; org-taskforecast-test.el ends here

;; Local Variables:
;; byte-compile-error-on-warn: t
;; End:
