# org-taskforecast ChangeLog

## development

[Commits](https://github.com/HKey/org-taskforecast/compare/0.1.0...master)

### Improvements

- Add header line formatter. ([#11](https://github.com/HKey/org-taskforecast/pull/11))
  - Add a custom variable `org-taskforecast-list-header-formatters`
- Add a command to move a task link into a section. ([#10](https://github.com/HKey/org-taskforecast/pull/10))
  - New command `org-taskforecast-list-move-link-to-section` can be called by `M`
- Add a command to go to the head todo task link of task links. ([#9](https://github.com/HKey/org-taskforecast/pull/9))
  - New command `org-taskforecast-list-goto-head-todo` can be called by `J`
- Show outline path in list buffer. ([#7](https://github.com/HKey/org-taskforecast/pull/7), [#8](https://github.com/HKey/org-taskforecast/pull/8))
  - Add custom variables
    - `org-taskforecast-list-show-outline-path`
    - `org-taskforecast-list-show-outline-path-delay`
- Make registration filter customizable. ([#6](https://github.com/HKey/org-taskforecast/pull/6))
  - Add a custom variable `org-taskforecast-registration-filter-function`
  - Make functions obsolete
    - `org-taskforecast-task-scheduled-planned-date-p`
    - `org-taskforecast-task-deadline-planned-date-p`
- Add consideration for warning days of DEADLINE when searching tasks by `org-taskforecast-register-tasks-for-today`. ([#4](https://github.com/HKey/org-taskforecast/pull/4))
- Add a custom variable, `org-taskforecast-search-files`, files to be searched by `org-taskforecast-register-tasks-for-today`. ([#3](https://github.com/HKey/org-taskforecast/pull/3))

### Fixes

- Fix blank line remained by calling `org-taskforecast-list-remove-entry`. ([#5](https://github.com/HKey/org-taskforecast/pull/5))
- Fix restoring cursor position when it is at EOB when refreshing. ([#2](https://github.com/HKey/org-taskforecast/pull/2))

## 0.1.0 (2020/06/02)

- First release
