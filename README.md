lorg-calendar.el
================

    INSTRUCTIONS:  M-x lorg-calendar-generate

    To scroll month by month forward, use:  >
    To scroll month by month backward, use:  <
    To mark a new date not yet selected, use the left click on the mouse,
      which will also copy the date to the kill-ring.  Left-clicking on a
      date previously marked with the left-click will remove the overlay.

`lorg-calendar` is a simplified variation of a custom calendar that I use daily, which is on Github just in case anyone is interested in seeing how it works.  It does not come with any instructions other than the first few lines in the comments at the beginning of the file (same as above).  It is an Emacs 3-month or 12-month calendar that scrolls (forwards/backwards in time) and is able to mark birthdays, holidays, appointments, court appearances, and dates previously marked with the mouse.

Try using `lorg-calendar` with `emacs -q` -- i.e., nothing extra except for this library.  After the user verifies that `lorg-calendar` works as advertised (with no user configuration), feel free to expand upon and modify the example.

The 12-month scrolling calendar is a culmination of help that I received from several people who responded to various threads on stackoverflow.com regarding algorithms and so forth.  Where help was provided, I have included citations in the comments of the code.  It relies upon many of the existing functions/features of the Emacs built-in `calendar` and `org-mode` libraries.

The example of calendared dates (birthdays, appointments and court appearances) are in the month of **February 2014**.  The pre-programmed holidays (that automatically appear for all years) are the ones recognized by the Los Angeles Superior Court in California.

In my own setup (not available to the public) I use a custom modified version of `calfw`, which shows 1 month, 2 weeks, 1 week, or 1 day.  If I want to see 3 months or 12 months, then I use a more complex version of `lorg-calendar`, which is able to generate agenda views when I click on a date.

I synchronize my tasks/events with Toodledo over the internet with a custom modified version of `org-toodledo` (not available to the public), which supports all keywords and priorities (plus an extra field or two such as location); and, that is why the tasks/events in the example are in a specific format.

Here is a link to an even more simplified version of the 12-month scrolling calendar that I posted on stackoverflow.com:  http://stackoverflow.com/a/21409154/2112489

**12-MONTH CALENDAR -- SCROLLS BY MONTH (FORWARDS / BACKWARDS)**

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Scroll a yearly calendar by month -- in a forwards or backwards direction. ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (eval-after-load "calendar" '(progn
      (define-key calendar-mode-map "<" 'lawlist-scroll-year-calendar-backward)
      (define-key calendar-mode-map ">" 'lawlist-scroll-year-calendar-forward)))

    (defun year-calendar (&optional month year)
      "Generate a one (1) year calendar that can be scrolled by month in each direction.
    This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
    See also:  http://ivan.kanis.fr/caly.el"
    (interactive)
      (require 'calendar)
      (let* ((current-year (number-to-string (nth 5 (decode-time (current-time)))))
             (month (if month month
               (string-to-number
                 (read-string "Please enter a month number (e.g., 1):  " nil nil "1"))))
             (year (if year year
               (string-to-number
                 (read-string "Please enter a year (e.g., 2014):  "
                   nil nil current-year)))))
        (switch-to-buffer (get-buffer-create calendar-buffer))
        (when (not (eq major-mode 'calendar-mode))
          (calendar-mode))
        (setq displayed-month month)
        (setq displayed-year year)
        (setq buffer-read-only nil)
        (erase-buffer)
        ;; horizontal rows
        (calendar-for-loop j from 0 to 3 do
          ;; vertical columns
          (calendar-for-loop i from 0 to 2 do
            (calendar-generate-month
              ;; month
              (cond
                ((> (+ (* j 3) i month) 12)
                  (- (+ (* j 3) i month) 12))
                (t
                  (+ (* j 3) i month)))
              ;; year
              (cond
                ((> (+ (* j 3) i month) 12)
                 (+ year 1))
                (t
                  year))
              ;; indentation / spacing between months
              (+ 5 (* 25 i))))
          (goto-char (point-max))
          (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
          (widen)
          (goto-char (point-max))
          (narrow-to-region (point-max) (point-max)))
        (widen)
        (goto-char (point-min))
        (setq buffer-read-only t)))

    (defun lawlist-scroll-year-calendar-forward (&optional arg event)
      "Scroll the yearly calendar by month in a forward direction."
      (interactive (list (prefix-numeric-value current-prefix-arg)
                         last-nonmenu-event))
      (unless arg (setq arg 1))
      (save-selected-window
        (if (setq event (event-start event)) (select-window (posn-window event)))
        (unless (zerop arg)
          (let* (
                (month displayed-month)
                (year displayed-year))
            (calendar-increment-month month year arg)
            (year-calendar month year)))
        (goto-char (point-min))
        (run-hooks 'calendar-move-hook)))

    (defun lawlist-scroll-year-calendar-backward (&optional arg event)
      "Scroll the yearly calendar by month in a backward direction."
      (interactive (list (prefix-numeric-value current-prefix-arg)
                         last-nonmenu-event))
      (lawlist-scroll-year-calendar-forward (- (or arg 1)) event))
___

![screenshot](https://www.lawlist.com/images/calendar_example.png)