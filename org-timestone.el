;;; org-timestone.el --- Convince Org mode that you're at a different point in time. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alex Balgavy

;; Author: Alex Balgavy
;; Homepage: https://github.com/thezeroalpha/org-timestone.el
;; Keywords: org

;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Sometimes, I forget to mark tasks as completed when I actually do
;; them, especially with habits. This package lets you set a different
;; effective date and time for Org mode on a per-buffer basis, so you
;; can complete tasks and take notes on a different date and time
;; (e.g. yesterday, a year ago, one week into the future...).

;; This package provides a function to change the current time in Org
;; mode, `org-timestone-set-org-current-time-effective`. You can bind
;; it yourself.

;; A buffer-local variable containing the current selected datetime.
(defvar-local org-timestone--current-time-effective nil
  "If set, the current time that should be used when marking items in Org mode as done, taking notes, repeating tasks, etc.")

;; A function to manipulate that variable.
(defun org-timestone-set-org-current-time-effective ()
  "Set `current-time' in the current buffer for `org-todo'.
  Use `keyboard-quit' to unset it."
  (interactive)
  (setq org-timestone--current-time-effective
        (condition-case nil
            (org-read-date t 'totime)
          (quit nil))))

;; For the repeater (e.g. +1d, .+2m...)
(defun org-timestone--org-today-effective (old-org-today)
  (if org-timestone--current-time-effective
      (time-to-days org-timestone--current-time-effective)
    (funcall old-org-today)))

(advice-add 'org-today :around #'org-timestone--org-today-effective)

;; For logging (the timestamp that you see in the logbook with e.g. logrepeat)
(defun org-timestone--current-time-effective (old-org-current-time &rest args)
  "Return the manually set effective time, or call the original function to get it."
  (or org-timestone--current-time-effective
      (apply old-org-current-time args)))

(advice-add 'org-current-time :around #'org-timestone--current-time-effective)

;; For the LAST_REPEAT property. Determined from line 10482 in org.el,
;; function org-auto-repeat-maybe.
(defun org-timestone--org-time-stamp-format-effective (old-org-time-stamp-format &rest args)
  (if org-timestone--current-time-effective
      (format-time-string (apply old-org-time-stamp-format args) org-timestone--current-time-effective)
    (apply old-org-time-stamp-format args)))

(advice-add 'org-time-stamp-format :around #'org-timestone--org-time-stamp-format-effective)

(provide 'org-timestone)
;;; org-timestone.el ends here
