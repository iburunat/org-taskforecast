# org-taskforecast ChangeLog

## development

[Commits](https://github.com/HKey/org-taskforecast/compare/0.1.0...master)

- Make registration filter customizable. ([#6](https://github.com/HKey/org-taskforecast/pull/6))
  - Add a custom variable `org-taskforecast-registration-filter-function`
  - Make functions obsolete
    - `org-taskforecast-task-scheduled-planned-date-p`
    - `org-taskforecast-task-deadline-planned-date-p`
- Fix blank line remained by calling `org-taskforecast-list-remove-entry`. ([#5](https://github.com/HKey/org-taskforecast/pull/5))
- Add consideration for warning days of DEADLINE when searching tasks by `org-taskforecast-register-tasks-for-today`. ([#4](https://github.com/HKey/org-taskforecast/pull/4))
- Add a custom variable, `org-taskforecast-search-files`, files to be searched by `org-taskforecast-register-tasks-for-today`. ([#3](https://github.com/HKey/org-taskforecast/pull/3))
- Fix restoring cursor position when it is at EOB when refreshing. ([#2](https://github.com/HKey/org-taskforecast/pull/2))

## 0.1.0 (2020/06/02)

- First release
