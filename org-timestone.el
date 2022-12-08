;;; org-timestone.el --- Convince Org mode that you're at a different point in time -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alex Balgavy

;; Author: Alex Balgavy
;; Homepage: https://github.com/thezeroalpha/org-timestone.el
;; Keywords: org outlines

;; Package-Version: 0.2.1
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; Sometimes, I forget to mark tasks as completed when I actually do
;; them, especially with habits. This package lets you set a different
;; effective date and time for Org mode on a per-buffer basis, so you
;; can complete tasks and take notes on a different date and time
;; (e.g. yesterday, a year ago, one week into the future...).

;; This package provides a function to change the current time in Org
;; mode, `org-timestone-set-org-current-time-effective`. You can bind
;; it yourself.

;;; Code:
(require 'org)
(defvar-local org-timestone--current-time-effective nil
  "A buffer-local variable containing the current selected datetime.
If set, the current time that should be used when marking items
 in Org mode as done, taking notes, repeating tasks, etc.")

(defun org-timestone-set-org-current-time-effective ()
  "Set `current-time' in the current buffer for `org-todo'.
Use `keyboard-quit' to unset it."
  (interactive)
  (setq org-timestone--current-time-effective
        (condition-case nil
            (org-read-date t 'totime)
          (quit nil))))

(defun org-timestone-org-todo-wrapper (&optional arg)
  "Wrapper for org-todo.
Lets you set a specific time for state change with
\\[universal-argument] \\[universal-argument]
\\[universal-argument] \\[universal-argument] prefix ARG."
  (interactive "P")
  (cond ((equal arg '(256))
         (setq org-timestone--current-time-effective (org-read-date t 'totime))
         (unwind-protect
             (org-todo)
           (setq org-timestone--current-time-effective nil)))
        (t (org-todo arg))))

(defun org-timestone--org-today-effective (old-org-today)
  "Wrapper around org-today-effective.
Returns manually set time, or calls OLD-ORG-TODAY. Has to be
wrapped for the repeater (e.g. +1d, .+2m...) to work properly."
  (if org-timestone--current-time-effective
      (time-to-days org-timestone--current-time-effective)
    (funcall old-org-today)))

(advice-add #'org-today :around #'org-timestone--org-today-effective)

(defun org-timestone--current-time-effective (old-org-current-time &rest args)
  "Wrapper around current-time-effective.
Return the manually set effective time, or calls
 OLD-ORG-CURRENT-TIME with ARGS to get it. Has to be wrapped for
 logging (the timestamp that you see in the logbook with e.g.
 logrepeat) to work properly."
  (or org-timestone--current-time-effective
      (apply old-org-current-time args)))

(advice-add #'org-current-time :around #'org-timestone--current-time-effective)

(provide 'org-timestone)
;;; org-timestone.el ends here
