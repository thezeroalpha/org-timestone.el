# org-timestone.el: convince Org mode that it's a different day and time
Sometimes I forget to mark tasks as completed when I actually did them, especially habits.
This script lets you change the current date and time as perceived by Org mode, so you can take notes and complete tasks on a different day.
The customized date affects the repeater, logging of notes, and the `LAST_REPEAT` property.

## Installation
Several options:

- Download it directly and put it in your `load-path`
- Use quelpa:

    ```emacs-lisp
    (quelpa '(org-timestone :repo "thezeroalpha/org-timestone.el" :fetcher github))
    ```
- Use whatever package manager you prefer, as long as you can install from a Github source

## Configuration
By default, there are no key bindings.
The only interface to the package is the function `org-timestone-set-org-current-time-effective`, which you'll probably want to bind to a key.
For example, for Org mode only:

``` emacs-lisp
(add-hook 'org-mode-hook
    (lambda ()
        (define-key org-mode-map (kbd "C-c q t") #'org-timestone-set-org-current-time-effective)))
```

## Usage
Run `org-timestone-set-org-current-time-effective`, and you'll be prompted for a date.
If you enter a date, Org mode will think that date is the current date.
If you run `keyboard-quit` from inside the prompt (e.g. with `C-g`), you will unset the date (so Org mode will use the _actual_ current time again).
