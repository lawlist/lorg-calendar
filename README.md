lorg-calendar.el
================

    INSTRUCTIONS:

    To generate a 12-month rotating calendar _without_ sample events,
    type:  M-x lawlist-year-calendar

    To generate a 12-month rotating calendar _with_ sample events,
    type:  M-x lorg-calendar-generate

    To scroll month by month forward, use:  >

    To scroll month by month backward, use:  <

    To mark a new date not yet selected, use the left click on the mouse,
      which will also copy the date to the kill-ring.  Left-clicking on a
      date previously marked with the left-click will remove the overlay.

[This example has been tested and is working with Emacs version 28.2.]

`lorg-calendar` is a simplified variation of a custom calendar that I use daily, which is on Github just in case anyone is interested in seeing how it works.  It does not come with any instructions other than the first few lines in the comments at the beginning of the file (same as above).  It is an Emacs 3-month or 12-month calendar that scrolls (forwards/backwards in time) and is able to mark birthdays, holidays, appointments, court appearances, and dates previously marked with the mouse.

Try using `lorg-calendar` with `emacs -q` -- i.e., nothing extra except for this library.  After the user verifies that `lorg-calendar` works as advertised (with no user configuration), feel free to expand upon and modify the example.

The 12-month scrolling calendar is a culmination of help that I received from several people who responded to various threads on stackoverflow.com regarding algorithms and so forth.  Where help was provided, I have included citations in the comments of the code.  It relies upon many of the existing functions/features of the Emacs built-in `calendar` and `org-mode` libraries.

The example of calendared dates (birthdays, appointments and court appearances) are in the month of **February 2014**.  The pre-programmed holidays (that automatically appear for all years) are the ones recognized by the Los Angeles Superior Court in California.

In my own setup (not available to the public) I use a custom modified version of `calfw`, which shows 1 month, 2 weeks, 1 week, or 1 day.  If I want to see 3 months or 12 months, then I use a more complex version of `lorg-calendar`, which is able to generate agenda views when I click on a date.

I synchronize my tasks/events with Toodledo over the internet with a custom modified version of `org-toodledo` (not available to the public), which supports all keywords and priorities (plus an extra field or two such as location); and, that is why the tasks/events in the example are in a specific format.

For those users looking to create a custom calendar view, here is an example of how to extract the raw data that goes into creating an `*Org-Agenda*` view:

    ;;; https://emacs.stackexchange.com/a/12563/2287

    ;;; The following is a condensed example of how to extract the data that goes
    ;;; into an `*Org Agenda*` buffer when normally using `org-agenda-list`, with
    ;;; `org-agenda-entry-types` such as `:deadline`, `:scheduled`, `:timestamp`,
    ;;; `sexp`, `:deadline*`, and `:scheduled*`.  The range of dates -- `begin`
    ;;; and `end` -- should be in a Gregorian list format -- e.g., `'(6 1 2015)`.
    ;;; The customizable let-bound options are `org-agenda-prefix-format` and
    ;;; `org-agenda-entry-types`.  The function returns a result in the format of
    ;;;  a list.

    (require 'calendar)
    (require 'org)
    (require 'org-agenda)
    (require 'cl)

    ;; Portions of following code were extracted from:
    ;;   https://github.com/kiwanami/emacs-calfw written by Masashi Sakurai
    ;; Said code has been modified by @lawlist hereinbelow.
    ;;
    (defun org-get-entries-fn (begin end)
    "Return org schedule items between BEGIN and END.
    USAGE:  (org-get-entries-fn '(6 1 2015) '(12 31 2020))"
      (unless
          (and
            (calendar-date-is-valid-p begin)
            (calendar-date-is-valid-p end))
        (let ((debug-on-quit nil))
          (signal 'quit '("One or both of your Gregorian dates are invalid."))))
      (let ((org-agenda-buffer nil) ;; prevent error from `org-compile-prefix-format'
            ;; The variable `org-agenda-only-exact-dates' is apparently not operational.
            (org-scheduled-past-days 0) ;; avoid duplicate entries for overdue items
            (org-agenda-prefix-format "• ")
            (org-agenda-entry-types '(:scheduled))
            (date-after
              (lambda (date num)
                "Return the date after NUM days from DATE."
                (calendar-gregorian-from-absolute
                 (+ (calendar-absolute-from-gregorian date) num))))
            (enumerate-days
              (lambda (begin end)
                "Enumerate date objects between BEGIN and END."
                (when (> (calendar-absolute-from-gregorian begin)
                         (calendar-absolute-from-gregorian end))
                  (error "Invalid period : %S - %S" begin end))
                (let ((d begin) ret (cont t))
                  (while cont
                    (push (copy-sequence d) ret)
                    (setq cont (not (equal d end)))
                    (setq d (funcall date-after d 1)))
                  (nreverse ret))))
            result)
        (org-compile-prefix-format nil)
        (setq result
          (loop for date in (funcall enumerate-days begin end) append
            (loop for file in (org-agenda-files nil 'ifmode) append
              (progn
                (org-check-agenda-file file)
                (apply 'org-agenda-get-day-entries file date org-agenda-entry-types)))))
        result))

___

![Example](https://www.lawlist.com/images/calendar_example.png)
