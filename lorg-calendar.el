;; Scroll a yearly calendar by month -- in a forwards or backwards direction.

;; INSTRUCTIONS:

;; M-x lorg-calendar-generate
;;
;; To scroll month by month forward, use:  >
;; To scroll month by month backward, use:  <
;; To mark a new date not yet selected, use the left click on the mouse,
;;   which will also copy the date to the kill-ring.  Left-clicking on a
;;   date previously marked with the left-click will remove the overlay.

(require 'calendar)
(require 'org)
(require 'org-element)

(eval-after-load "calendar" '(progn
  (define-key calendar-mode-map "<" 'lorg-calendar-backward-month)
  (define-key calendar-mode-map ">" 'lorg-calendar-forward-month)
  (define-key calendar-mode-map "." 'lorg-calendar-goto-today)
  (define-key calendar-mode-map [mouse-1] 'lorg-mark-mouse-set-point) ))


(defvar lorg-calendar-buffer "*Calendar*"
  "This is the `buffer-name` of the calendar buffer.")

(defvar lorg-calendar-style nil
"A symbol -- 'twelve-months or 'three-months")
(make-variable-buffer-local 'lorg-calendar-style)

(defun lorg-calendar-style-function ()
  (cond
    (lorg-calendar-style lorg-calendar-style)
    ((get-buffer-window lorg-calendar-buffer (selected-frame))
      (with-current-buffer (get-buffer lorg-calendar-buffer)
        lorg-calendar-style))
    (t 'three-months)))

(defun lorg-calendar-generate (&optional month year)
  "Generate a 3-month or 12-month Gregorian calendar.  The `displayed-month`
and `displayed-year` in a 3-month calendar are the month and year in the
center.  The `displayed-month` and `displayed-year` in a 12-month calendar
are the month and year in the upper left-hand corner of the buffer.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html"
(interactive)
  (let* (
      (org-startup-folded nil)
      (today (lorg-calendar-current-date))
      (current-year (number-to-string (nth 5 (decode-time (current-time)))))
      (month
        (cond
          ((called-interactively-p)
            (read-number
              "Please choose the `displayed-month` (1 through 12):  " 1))
          (t
            (if month month (calendar-extract-month today)))))
      (year
        (cond
          ((called-interactively-p)
            (read-number
              "Please choose the `displayed-year` (e.g., 2014):  " current-year))
          (t
            (if year year (calendar-extract-year today)))))
      (style
        (cond
          ((called-interactively-p)
            (lorg-message
              "Please choose letter a or b:  [a] 3-month calendar; or, [b] 12-month calendar ?")
            (let* ((selected-calendar-style (read-char-exclusive)))
              (cond
                ((eq selected-calendar-style ?a)
                  'three-months)
                ((eq selected-calendar-style ?b)
                  'twelve-months)
                (t
                  'twelve-months) )))
          (t
            (lorg-calendar-style-function))))
      (lorg-court-holiday t)
      (appointment t)
      (lorg-birthday t)
      (lorg-court-appearance t)
      lorg-court-appearances
      lorg-birthdays
      lorg-appointments
      narrowed
      element
      todo-state
      deadline
      scheduled
      day
      title
      (old-date
        (when (get-buffer lorg-calendar-buffer)
          (with-current-buffer (get-buffer lorg-calendar-buffer)
            (lorg-calendar-cursor-to-nearest-date))))
      (org-todo-keywords '((sequence
       "Active(a)"
       "Next Action(n)"
       "Canceled(c)"
       "Hold(h)"
       "Reference(r)"
       "Delegated(d)"
       "Waiting(w)"
       "Postponed(P)"
       "Someday(s)"
       "Planning(p)"
       "|"
       "None(N)") ))
      (sample-todo (concat
        "* TASKS\n\n"
        "** Active [#A] albert @ Dune (Frank Herbert). :albert:@context_one:\n"
        "   DEADLINE: <2013-12-21 Sat 17:00>  SCHEDULED: <2013-12-21 Sat>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 15\n"
        "   :Hash:\n"
        "   :ToodledoFolder: TASKS\n"
        "   :ToodledoGoal: Goal #1\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #1\n"
        "   :END:\n\n"
        "** Next Action [#B] 0 @ Ender's Game (Orson Scott Card). :lorg:@context_two:\n"
        "   DEADLINE: <2014-02-22 Sat 08:00>  SCHEDULED: <2014-02-22 Sat>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 30\n"
        "   :Hash:\n"
        "   :ToodledoFolder: TASKS\n"
        "   :ToodledoGoal: Goal #2\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #2\n"
        "   :END:\n"
        "• This novel was of particular interest because . . .\n"
        "• Orson Scott Card has written other books . . . \n"
        "• Compare and contrast Ender's brother and sister.\n\n"
        "** Hold [#C] wells @ War of the Worlds (H G Wells). :lorg:@context_three:\n"
        "   DEADLINE: <2014-04-03 Thu 08:30>  SCHEDULED: <2014-04-03 Thu>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 45\n"
        "   :Hash:\n"
        "   :ToodledoFolder: TASKS\n"
        "   :ToodledoGoal: Goal #3\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #3\n"
        "   :END:\n\n"
        "** Canceled [#D] wells @ The Invisible Man (H G Wells). :lorg:@context_four:\n"
        "   DEADLINE: <2014-04-07 Mon>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 60\n"
        "   :Hash:\n"
        "   :ToodledoFolder: TASKS\n"
        "   :ToodledoGoal: Goal #4\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #4\n"
        "   :END:\n\n"
        "* EVENTS\n\n"
        "** Delegated [#A] candice @ meeting -- February 8, 2014; 08:00. :candice:@context_five:\n"
        "   DEADLINE: <2014-02-08 Sat 08:00>  SCHEDULED: <2014-02-08 Sat>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 75\n"
        "   :Hash:\n"
        "   :ToodledoFolder: EVENTS\n"
        "   :ToodledoGoal: Goal #5\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #5\n"
        "   :END:\n\n"
        "** Postponed [#B] george @ 2001 - A Space Odyssey (Arthur C Clarke). :george:@context_six:\n"
        "   DEADLINE: <2014-02-12 Wed>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 90\n"
        "   :Hash:\n"
        "   :ToodledoFolder: EVENTS\n"
        "   :ToodledoGoal: Goal #6\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #6\n"
        "   :END:\n\n"
        "** Waiting [#C] edward @ birthday -- February 15, 2014. :edward:@context_seven:\n"
        "   DEADLINE: <2014-02-15 Sat>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 105\n"
        "   :Hash:\n"
        "   :ToodledoFolder: EVENTS\n"
        "   :ToodledoGoal: Goal #7\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #7\n"
        "   :END:\n\n"
        "** Reference [#D] fred @ lorg-court-appearance -- February 26, 2014 at 8:30 a.m. :fred:@context_eight:\n"
        "   DEADLINE: <2014-02-26 Wed 08:30>  SCHEDULED: <2014-02-26 Wed>\n"
        "   :PROPERTIES:\n"
        "   :Effort: 120\n"
        "   :Hash:\n"
        "   :ToodledoFolder: EVENTS\n"
        "   :ToodledoGoal: Goal #8\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #8\n"
        "   :END:\n\n"
        "* UNDATED\n\n"
        "** Someday [#D] harold @ The Foundation Trilogy (Isaac Asimov). :harold:@context_nine:\n"
        "   :PROPERTIES:\n"
        "   :Effort: 135\n"
        "   :Hash:\n"
        "   :ToodledoFolder: UNDATED\n"
        "   :ToodledoGoal: Goal #9\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #9\n"
        "   :END:\n\n"
        "* CONTACTS\n\n"
        "** Planning [#D] John H. Smith  :smith:@context_ten:\n"
        "   :PROPERTIES:\n"
        "   :Effort: 150\n"
        "   :Hash:\n"
        "   :ToodledoFolder: CONTACTS\n"
        "   :ToodledoGoal: Goal #10\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #10\n"
        "   :END:\n"
        "1234 Civic Center Drive, Suite 300\n"
        "New York, NY  12345\n"
        "  work:  (212) 123-4567\n"
        "  email:  nobody@nowhere.com\n\n"
        "* DONE\n\n"
        "** None [#E] 0 @ Provide a working example on StackOverflow. :lorg:@context_eleven:\n"
        "   :PROPERTIES:\n"
        "   :Effort: 165\n"
        "   :Hash:\n"
        "   :ToodledoFolder: DONE\n"
        "   :ToodledoGoal: Goal #11\n"
        "   :ToodledoID:\n"
        "   :ToodledoLocation: Location #11\n"
        "   :END:")))
    (with-current-buffer (get-buffer-create lorg-calendar-buffer)
      (when (not (eq major-mode 'calendar-mode))
        (calendar-mode))
      (setq displayed-month month)
      (setq displayed-year year)
      (setq buffer-read-only nil)
      (erase-buffer)
      (cond
        ((eq style 'twelve-months)
          (setq lorg-calendar-style 'twelve-months)
          ;; horizontal row
          (lorg-calendar-for-loop j from 0 to 3 do
            ;; vertical column
            (lorg-calendar-for-loop i from 0 to 2 do
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
          (widen) )
        ((eq style 'three-months)
          (setq lorg-calendar-style 'three-months)
          (if (< (+ month (* 12 (1- year))) 2)
              (error "Months before January, 1 AD cannot be displayed"))
          (calendar-increment-month month year -1)
          (dotimes (i 3)
            (calendar-generate-month month year (+ calendar-left-margin (* calendar-month-width i)))
            (calendar-increment-month month year 1))))
      (setq buffer-read-only t)
      (setq cursor-type '(hbar . 2))
      (calendar-update-mode-line) )
    ;; lorg mark calendar -- BEGIN
    (if (get-buffer "foo.org")
      (with-current-buffer "foo.org"
        (erase-buffer))
      (with-current-buffer (get-buffer-create "foo.org")
        (org-mode)))
    (with-current-buffer (get-buffer "foo.org")
      (insert sample-todo)
      (goto-char (point-min))
      (save-excursion
        (when (lorg-narrow-p)
          (setq narrowed t)
          (widen))
        (goto-char (point-max))
        (while (re-search-backward "^\\*\\* \\(Reference\\|Delegated\\|Waiting\\)" nil t)
          (setq element (org-element-at-point))
          (setq todo-state (org-element-property :todo-keyword element))
          (setq title (org-element-property :title element))
          (setq deadline
            (ignore-errors
              (org-element-property :deadline element) ))
          (setq scheduled
            (ignore-errors
              (org-element-property :scheduled element) ))
          (setq day (ignore-errors (org-element-property :day-start deadline)))
          (setq month (ignore-errors (org-element-property :month-start deadline)))
          (setq year (ignore-errors (org-element-property :year-start deadline)))
          (when (and
                  scheduled
                  deadline
                  (string= todo-state "Reference"))
            ;; formatting of `lorg-cal-sexp` code provided by @phils
            ;; http://stackoverflow.com/q/20715445/2112489
            (setq lorg-court-appearances
              (append lorg-court-appearances
                `((lorg-cal-sexp '(list ,month ,day ,year) (regexp-quote ,title))))))
          (when (and
                  scheduled
                  deadline
                  (string= todo-state "Delegated"))
            ;; formatting of `lorg-cal-sexp` code provided by @phils
            ;; http://stackoverflow.com/q/20715445/2112489
            (setq lorg-appointments
              (append lorg-appointments
                `((lorg-cal-sexp '(list ,month ,day ,year) (regexp-quote ,title))))))
          (when (and
                  deadline
                  (string= todo-state "Waiting"))
            ;; formatting of `lorg-cal-sexp` code provided by @phils
            ;; http://stackoverflow.com/q/20715445/2112489
            (setq lorg-birthdays
              (append lorg-birthdays
                `((lorg-cal-sexp '(list ,month ,day ,year) (regexp-quote ,title))))))  ))
      (if narrowed (org-narrow-to-subtree)))
    (with-current-buffer (get-buffer lorg-calendar-buffer)
      ;; lorg-court-holidays
      (dolist (holiday 
        (let (res h err)
          (sort
            (dolist (p lorg-court-holidays res)
              (if (setq h (eval p))
                 (setq res (append h res))))
            'calendar-date-compare)))
        (lorg-calendar-mark-visible-date (car holiday) lorg-court-holiday-face nil lorg-court-holiday nil nil nil))
      ;; lorg-court-appearances
      (dolist (holiday 
        (let (res h err)
          (sort
            (dolist (p lorg-court-appearances res)
              (if (setq h (eval p))
                 (setq res (append h res))))
        'calendar-date-compare)))
        (lorg-calendar-mark-visible-date (car holiday) lorg-court-appearance-face nil nil lorg-court-appearance nil nil))
      ;; lorg-appointments
      (dolist (holiday
        (let (res h err)
          (sort
            (dolist (p lorg-appointments res)
              (if (setq h (eval p))
                 (setq res (append h res))))
        'calendar-date-compare)))
        (lorg-calendar-mark-visible-date (car holiday) lorg-appointment-face nil nil nil appointment nil)) 
      ;; lorg-birthdays
      (dolist (holiday
        (let (res h err)
          (sort
            (dolist (p lorg-birthdays res)
              (if (setq h (eval p))
                 (setq res (append h res))))
        'calendar-date-compare)))
        (lorg-calendar-mark-visible-date (car holiday) lorg-birthday-face nil nil nil nil lorg-birthday))
      ;; lorg-mouse-calendar-face markings
      (dolist (holiday
        (let (res h err)
          (sort
            (dolist (p lorg-mouse-marked res)
              (if (setq h (eval p))
                 (setq res (append h res))))
            'calendar-date-compare)))
        (lorg-calendar-mark-visible-date (car holiday) lorg-mouse-calendar-face))
    ;; lorg mark calendar -- END
      (cond
        ((and
            old-date
            (lorg-calendar-date-is-visible-p old-date))
          (lorg-calendar-cursor-to-visible-date old-date))
        ((lorg-calendar-date-is-visible-p today)
          (lorg-calendar-cursor-to-visible-date today))
        (t
          (lorg-calendar-cursor-to-visible-date (list displayed-month 1 displayed-year)))))
    (cond
      ((eq style 'twelve-months)
        (lorg-display-buffer-right (get-buffer lorg-calendar-buffer) '((window-width . 82))))
      ((eq style 'three-months)
        (lorg-display-buffer-below (get-buffer lorg-calendar-buffer) '((window-height . 12)))))
    (lorg-display-buffer-left (get-buffer "foo.org") nil)
    (select-window (get-buffer-window lorg-calendar-buffer)) ))

(defvar lorg-mouse-marked nil
 "A list of the calendar dates that have been marked with the mouse.")
(make-variable-buffer-local 'lorg-mouse-marked)

(defun lorg-mouse-marked ()
  (let* (
      lorg-mouse-marked-date
      lorg-mouse-marked-day
      lorg-mouse-marked-month
      lorg-mouse-marked-year)
    (setq lorg-mouse-marked nil)
    (when (get-buffer lorg-calendar-buffer)
      (with-current-buffer (get-buffer lorg-calendar-buffer)
        (save-excursion
          (goto-char (point-max))
          (while (re-search-backward "[0-9]" nil t)
            (when
              (memq lorg-mouse-calendar-face
                (mapcar (function (lambda (ovr)
                  (overlay-get ovr 'face)))  (overlays-at (point))))
              (setq lorg-mouse-marked-date (lorg-calendar-cursor-to-nearest-date))
              (setq lorg-mouse-marked-day (calendar-extract-day lorg-mouse-marked-date))
              (setq lorg-mouse-marked-month (calendar-extract-month lorg-mouse-marked-date))
              (setq lorg-mouse-marked-year (calendar-extract-year lorg-mouse-marked-date))
              ;; formatting of `lorg-cal-sexp` code provided by @phils
              ;; http://stackoverflow.com/q/20715445/2112489
              (setq lorg-mouse-marked
                (append lorg-mouse-marked
                  `((lorg-cal-sexp '(list
                    ,lorg-mouse-marked-month
                    ,lorg-mouse-marked-day
                    ,lorg-mouse-marked-year) "")))))  )) ))))

(defun lorg-mark-mouse-set-point (event)
(interactive "e")
  (mouse-set-point event)
  (if (not (overlays-at (point)))
    (progn
      (lorg-calendar-mark-visible-date (lorg-calendar-cursor-to-date t) lorg-mouse-calendar-face)
      (lorg-mouse-marked)
      (lorg-calendar-copy-date nil))
    (lorg-calendar-copy-date nil)
    (let* (
        beg
        end)
      (cond
        ;; This mapcar overlay snippet was borrowed from a function written by Drew Adams
        ;; `mark-visible-calendar-date` at http://www.emacswiki.org/emacs/calendar%2B.el
        ((memq lorg-mouse-calendar-face
            (mapcar (function (lambda (ovr)
              (overlay-get ovr 'face)))  (overlays-at (point))))
          (cond
            ;; cursor is one whitespace to the left of 1 to 9
            ((and
                (save-excursion (< 0 (skip-chars-forward " \t")))
                (not (save-excursion (< 0 (skip-syntax-forward "w")))))
              (save-excursion
                (setq beg (point))
                (skip-chars-forward " \t")
                (skip-syntax-forward "w")
                (setq end (point))
                (mapc 'delete-overlay (overlays-in beg end))))
            ;; cursor is sandwiched between a digit on each side.
            ((and
                (save-excursion (> 0 (skip-syntax-backward "w")))
                (save-excursion (< 0 (skip-syntax-forward "w"))))
              (save-excursion
                (skip-syntax-backward "w")
                (setq beg (point))
                (skip-syntax-forward "w")
                (setq end (point))
                (mapc 'delete-overlay (overlays-in beg end))))
            ;; cursor is to the far right of one or two digit dates
            ((and
                (save-excursion (> 0 (skip-syntax-backward "w")))
                (not (save-excursion (< 0 (skip-syntax-forward "w")))))
              (save-excursion
                (skip-syntax-backward "w")
                (setq beg (point))
                (skip-syntax-forward "w")
                (setq end (point))
                (mapc 'delete-overlay (overlays-in beg end))))
            ;; cursor to the far left of one or two digits dates
            ((and
                (save-excursion (< 0 (skip-syntax-forward "w")))
                (not (save-excursion (> 0 (skip-syntax-backward "w")))))
              (save-excursion
                (skip-syntax-forward "w")
                (setq end (point))
                (skip-syntax-backward "w")
                (setq beg (1- (point))) ;; I am using `1-' for days 1 to 9
                (mapc 'delete-overlay (overlays-in beg end)))) )
          (lorg-mouse-marked))
        (t
          (lorg-message "This fall-back feature has not yet been defined.") ) ))))

(defun lorg-calendar-mark-visible-date (date &optional mark priority lorg-court-holiday lorg-court-appearance lorg-appointment lorg-birthday)
"A modification of the function `mark-visible-calendar-date` by Drew Adams: http://www.emacswiki.org/emacs/calendar%2B.el"
  (when (calendar-date-is-valid-p date)
    (save-excursion
      (set-buffer lorg-calendar-buffer)
      (lorg-calendar-cursor-to-visible-date date)
      (let ((mark (or mark diary-entry-marker)))
        (if (stringp mark)
            (let ((buffer-read-only nil))
              (forward-char 1)
              (delete-char 1)
              (insert mark)
              (forward-char -2))
          (cond
           ((and
              lorg-appointment
              (memq lorg-court-appearance-face
                 (mapcar (function (lambda (ovr)
                   (overlay-get ovr 'face)))  (overlays-at (point)))) )
             (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
               (overlay-put ovrly 'face lorg-appointment-appearance-face)
               (overlay-put ovrly 'priority 1001)))
            ((and
                lorg-appointment
                (memq lorg-court-holiday-face
                  (mapcar (function (lambda (ovr)
                    (overlay-get ovr 'face)))  (overlays-at (point)))))
              (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
                (overlay-put ovrly 'face lorg-holiday-appointment-face)
                (overlay-put ovrly 'priority 1001)))
            ((and
                lorg-appointment
                (not (memq lorg-court-holiday-face
                  (mapcar (function (lambda (ovr)
                    (overlay-get ovr 'face)))  (overlays-at (point)))))
                (not (memq lorg-court-appearance-face
                   (mapcar (function (lambda (ovr)
                     (overlay-get ovr 'face)))  (overlays-at (point)))) ))
              (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
                (overlay-put ovrly 'face lorg-appointment-face)
                (overlay-put ovrly 'priority 1000)))
            (lorg-court-appearance
              (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
                (overlay-put ovrly 'face lorg-court-appearance-face)
                (overlay-put ovrly 'priority 1000)))
            ((and
                lorg-birthday
                (not (memq lorg-court-holiday-face
                  (mapcar (function (lambda (ovr)
                    (overlay-get ovr 'face)))  (overlays-at (point)))))
                (not (memq lorg-court-appearance-face
                   (mapcar (function (lambda (ovr)
                     (overlay-get ovr 'face)))  (overlays-at (point)))))
                (not (memq lorg-appointment-face
                   (mapcar (function (lambda (ovr)
                     (overlay-get ovr 'face)))  (overlays-at (point))))))
              (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
                (overlay-put ovrly 'face lorg-birthday-face)
                (overlay-put ovrly 'priority 1001)))
            ((and
                lorg-birthday
                (or
                  (memq lorg-court-holiday-face
                    (mapcar (function (lambda (ovr)
                      (overlay-get ovr 'face)))  (overlays-at (point))))
                  (memq lorg-court-appearance-face
                     (mapcar (function (lambda (ovr)
                       (overlay-get ovr 'face)))  (overlays-at (point))))
                  (memq lorg-appointment-face
                     (mapcar (function (lambda (ovr)
                       (overlay-get ovr 'face)))  (overlays-at (point))))))
              (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
                (overlay-put ovrly 'face lorg-birthday+others-face)
                (overlay-put ovrly 'priority 1001)))
            (t
              (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
               (overlay-put ovrly 'face mark)
               (when priority (overlay-put ovrly 'priority 1000))))  ))))))

(defun lorg-calendar-copy-date (arg)
  "http://www.emacswiki.org/emacs-se/IanYang
Copy date under the cursor.  Read format from minibuffer if ARG, use
recently used format if no ARG.  See the function `format-time-string' 
for the document of time format string"
  (interactive "P")
  (let ((date (lorg-calendar-cursor-to-date t))
        (format (if arg
                    (completing-read
                     "Date Format:"
                     (list "%B %d, %Y") nil nil nil
                     '(list "%B %d, %Y") nil nil)
                  (car (list "%B %d, %Y"))))
        string)
    (setq date (encode-time 0 0 0 (cadr date) (car date) (nth 2 date)))
    (setq string (format-time-string format date))
    (if (eq last-command 'kill-region)
        (kill-append string nil)
      (kill-new string))
    (message "%s" string)))

(defun lorg-calendar-date-is-visible-p (date)
  "Return non-nil if DATE is valid and is visible in the lorg-calendar window."
  (let ((lorg-calendar-style (lorg-calendar-style-function)))
    (cond
      ((eq lorg-calendar-style 'twelve-months)
        (let* (
            (incoming-test-date (calendar-absolute-from-gregorian date))
            (beginning-date (calendar-absolute-from-gregorian (list displayed-month 1 displayed-year)))
            (ending-date (+ 364 beginning-date)))
          (and
            (>= incoming-test-date beginning-date)
            (<= incoming-test-date ending-date))))
      (t
        (and (calendar-date-is-valid-p date)
             (< (abs (calendar-interval
                      displayed-month displayed-year
                      (calendar-extract-month date) (calendar-extract-year date)))
                2))))))

(defun lorg-calendar-cursor-to-date (&optional error event)
  "Return a list (month day year) of current cursor position.
If cursor is not on a specific date, signals an error if optional parameter
ERROR is non-nil, otherwise just returns nil.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (let ((lorg-calendar-style (lorg-calendar-style-function)))
    (cond
      ((eq lorg-calendar-style 'twelve-months)
        (let* ((column-segment (/ (current-column) 25))
               (line-segment (/ (count-lines (point-min) (point)) 9))
               (current-block (+ (+ (* line-segment 3) column-segment) 1))
               (tentative-month (- (+ current-block displayed-month) 1))
                (month
                    (cond
                      ((> tentative-month 12)
                        (- tentative-month 12))
                      (t
                        tentative-month)))
               (year
                    (cond
                      ((>= tentative-month 13)
                        (+ displayed-year 1))
                      (t
                        displayed-year))) )
          (if (and (looking-at "[ 0-9]?[0-9][^0-9]")
                   (< 2 (count-lines (point-min) (point))))
              (save-excursion
                (if (not (looking-at " "))
                    (re-search-backward "[^0-9]"))
                (list month
                      (string-to-int
                       (buffer-substring (1+ (point)) (+ 4 (point))))
                      year))
            (if (looking-at "\\*")
                (save-excursion
                  (re-search-backward "[^*]")
                  (if (looking-at ".\\*\\*")
                      (list month calendar-starred-day year)
                    (if error
                        (let ((debug-on-quit nil))
                          (signal 'quit `("Not on a date!"))))))
                    (if error
                        (let ((debug-on-quit nil))
                          (signal 'quit `("Not on a date!"))))))))
      (t
        (with-current-buffer
            (if event (window-buffer (posn-window (event-start event)))
              (current-buffer))
          (save-excursion
            (and event (setq event (event-start event))
                 (goto-char (posn-point event)))
            (let* ((segment (calendar-column-to-segment))
                   (month (% (+ displayed-month (1- segment)) 12)))
              ;; Call with point on either of the two digits in a 2-digit date,
              ;; or on or before the digit of a 1-digit date.
              (if (not (and (looking-at "[ 0-9]?[0-9][^0-9]")
                            (get-text-property (point) 'date)))
                (if error
                  (let ((debug-on-quit nil))
                    (signal 'quit `("Not on a date!"))))
                ;; Convert segment to real month and year.
                (if (zerop month) (setq month 12))
                ;; Go back to before the first date digit.
                (or (looking-at " ")
                    (re-search-backward "[^0-9]"))
                (list month
                      (string-to-number
                       (buffer-substring (1+ (point))
                                         (+ 1 calendar-day-digit-width (point))))
                      (cond
                       ((and (= 12 month) (zerop segment)) (1- displayed-year))
                       ((and (= 1 month) (= segment 2)) (1+ displayed-year))
                       (t displayed-year)))))))))))

(defun lorg-calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let ((lorg-calendar-style (lorg-calendar-style-function)))
    (cond
      ((eq lorg-calendar-style 'twelve-months)
        (let* (
            (month (calendar-extract-month date))
            (day (calendar-extract-day date))
            (year (calendar-extract-year date))
            (first-of-month-weekday (calendar-day-of-week (list month 1 year)))
            ;; algorithm for rowmove and colmove written by @AShelly
            ;; http://stackoverflow.com/a/21709710/2112489
            (difference
              (if (>= month displayed-month) 
                (- month displayed-month) 
                (- (+ month 12) displayed-month)))
            (rows (/ difference 3))
            (cols (% difference 3))
            (rowmove (* 9 rows))   
            (colmove (+ 6 (* 25 cols))) )
          (goto-line
            (+ 3
              rowmove
                (/ (+ day  -1
                  (mod
                    (- (calendar-day-of-week (list month 1 year)) calendar-week-start-day)
                      7))
                        7)))
          (move-to-column
            (+
              colmove
                (* 3 (mod
                  (- (calendar-day-of-week date) calendar-week-start-day)
                    7))))))
      (t
        (let* (
            (month (calendar-extract-month date))
            (day (calendar-extract-day date))
            (year (calendar-extract-year date)))
          (goto-char (point-min))
          (forward-line (+ calendar-first-date-row -1
                           (/ (+ day -1
                                 (mod
                                  (- (calendar-day-of-week (list month 1 year))
                                     calendar-week-start-day)
                                  7))
                              7)))
          (move-to-column (+ calendar-left-margin (1- calendar-day-digit-width)
                             (* calendar-month-width
                                (1+ (calendar-interval
                                     displayed-month displayed-year month year)))
                             (* calendar-column-width
                                (mod
                                 (- (calendar-day-of-week date)
                                    calendar-week-start-day)
                                 7)))))))
    (if (get-buffer-window lorg-calendar-buffer (selected-frame))
      (set-window-point (get-buffer-window lorg-calendar-buffer (selected-frame)) (point)))))

(defun lorg-target-year-function (target-month)
"Algorithm written by @legoscia on stackoverflow:
http://stackoverflow.com/a/21834918/2112489"
  (if (>= target-month displayed-month)
      displayed-year
    (1+ displayed-year)))

(defcustom lorg-court-holidays
  (mapcar 'purecopy
  '(
    (lorg-new-year-day)
    (lorg-holiday-float 1 1 3 "Martin Luther King Day")
    (lorg-lincoln-day)
    (lorg-holiday-float 2 1 3 "President's Day")
    (lorg-cesar-chavez-day)
    (lorg-holiday-float 5 1 -1 "Memorial Day")
    (lorg-independence-day)
    (lorg-holiday-float 9 1 1 "Labor Day")
    (lorg-holiday-float 10 1 2 "Columbus Day")
    (lorg-veterans-day)
    (lorg-holiday-float 11 4 4 "Thanksgiving")
    (lorg-day-after-thanksgiving)
    (lorg-christmas-day) ))
  "Court holidays."
  :type 'sexp
  :group 'holidays)

(defun lorg-new-year-day ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 1))
          (d (calendar-day-of-week (list 1 1 y))) )
        (cond 
          ((= d 6) ;; Saturday
            (list (list (list 1 3 y) "New Year + 2")))
          ((= d 0) ;; Sunday
            (list (list (list 1 2 y) "New Year + 1")))
          (t
            (list (list (list 1 1 y) "New Year"))))) )
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 1 1 y))) )
        (cond
          ((<= displayed-month 2)
            (cond 
              ((= d 6) ;; Saturday
                (list (list (list 1 3 y) "New Year + 2")))
              ((= d 0) ;; Sunday
                (list (list (list 1 2 y) "New Year + 1")))
              (t
                (list (list (list 1 1 y) "New Year")))))
          ((= displayed-month 12)
            (setq y (+ y 1))
            (setq d (calendar-day-of-week (list 1 1 y)))
            (cond 
              ((= d 6) ;; Saturday
                (list (list (list 1 3 y) "New Year + 2")))
              ((= d 0) ;; Sunday
                (list (list (list 1 2 y) "New Year + 1")))
              (t
                (list (list (list 1 1 y) "New Year"))))) )) )))

(defun lorg-lincoln-day ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 2))
          (d (calendar-day-of-week (list 2 12 y))) )
        (cond
          ((= d 6)
            (list (list (list 2 11 y) "Lincoln Day")))
          ((= d 0)
            (list (list (list 2 13 y) "Lincoln Day")))
          (t
            (list (list (list 2 12 y) "Lincoln Day")) ))))
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 2 12 y))) )
        (when (or (= displayed-month 1) (= displayed-month 2) (= displayed-month 3))
          (cond
            ((= d 6)
              (list (list (list 2 11 y) "Lincoln Day")))
            ((= d 0)
              (list (list (list 2 13 y) "Lincoln Day")))
            (t
              (list (list (list 2 12 y) "Lincoln Day")) )))))))

(defun lorg-cesar-chavez-day ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 3))
          (d (calendar-day-of-week (list 3 31 y))) )
        (cond
          ((= d 6)
            (list (list (list 3 30 y) "Cesar Chavez Day")))
          ((= d 0)
            (list (list (list 4 1 y) "Cesar Chavez Day")))
          (t
            (list (list (list 3 31 y) "Cesar Chavez Day")) ))))
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 3 31 y))) )
        (cond
          ((and (= d 6) (or (= displayed-month 2) (= displayed-month 3) (= displayed-month 4)))
            (list (list (list 3 30 y) "Cesar Chavez Day")))
          ((and (= d 0) (or (= displayed-month 3) (= displayed-month 4) (= displayed-month 5)))
            (list (list (list 4 1 y) "Cesar Chavez Day")))
          ((and (not (or (= d 6) (= d 0))) (or (= displayed-month 2) (= displayed-month 3) (= displayed-month 4)))
            (list (list (list 3 31 y) "Cesar Chavez Day")) ))))))

(defun lorg-independence-day ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 7))
          (d (calendar-day-of-week (list 7 4 y))) )
        (cond
	          ((= d 6)
            (list (list (list 7 3 y) "Independence Day")))
          ((= d 0)
            (list (list (list 7 5 y) "Independence Day")))
          (t
            (list (list (list 7 4 y) "Independence Day")) ))))
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 7 4 y))) )
        (when (or (= displayed-month 6) (= displayed-month 7) (= displayed-month 8))
          (cond
            ((= d 6)
              (list (list (list 7 3 y) "Independence Day")))
            ((= d 0)
              (list (list (list 7 5 y) "Independence Day")))
            (t
              (list (list (list 7 4 y) "Independence Day")) )))))))

(defun lorg-veterans-day ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 11))
          (d (calendar-day-of-week (list 11 11 y))) )
        (cond
          ((= d 6)
            (list (list (list 11 10 y) "Veteran's Day")))
          ((= d 0)
            (list (list (list 11 12 y) "Veteran's Day")))
          (t
            (list (list (list 11 11 y) "Veteran's Day")) ))))
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 11 11 y))) )
        (when (>= displayed-month 10)
          (cond
            ((= d 6)
              (list (list (list 11 10 y) "Veteran's Day")))
            ((= d 0)
              (list (list (list 11 12 y) "Veteran's Day")))
            (t
              (list (list (list 11 11 y) "Veteran's Day")) )))))))

(defun lorg-day-after-thanksgiving ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 11))
          (d (calendar-day-of-week (list 11 1 y))) )
        (cond
          ((= d 5)
            (list (list (list 11 29 y) "Day After Thanksgiving")))
          ((= d 6)
            (list (list (list 11 28 y) "Day After Thanksgiving")))
          (t
            (lorg-holiday-float 11 5 4 "Day After Thanksgiving")))) )
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 11 1 y))) )
        (when (>= displayed-month 10)
          (cond
            ((= d 5)
              (list (list (list 11 29 y) "Day After Thanksgiving")))
            (t
              (lorg-holiday-float 11 5 4 "Day After Thanksgiving"))))))))

(defun lorg-christmas-day ()
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* (
          (y (lorg-target-year-function 12))
          (d (calendar-day-of-week (list 12 25 y))) )
        (cond 
           ((= d 6) ;; Saturday
             (list (list (list 12 27 y) "Christmas Day + 2")))
           ((= d 0) ;; Sunday
             (list (list (list 12 26 y) "Christmas Day + 1")))
           (t
             (list (list (list 12 25 y) "Christmas Day"))))) )
    ((eq lorg-calendar-style 'three-months)
      (let* (
          (y displayed-year)
          (d (calendar-day-of-week (list 12 25 y))) )
        (cond
          ((>= displayed-month 11)
            (cond 
              ((= d 6) ;; Saturday
                (list (list (list 12 27 y) "Christmas Day + 2")))
              ((= d 0) ;; Sunday
                (list (list (list 12 26 y) "Christmas Day + 1")))
              (t
                (list (list (list 12 25 y) "Christmas Day")))))
          ((= displayed-month 1)
            (setq y (- y 1))
            (setq d (calendar-day-of-week (list 12 25 y)))
            (cond 
              ((= d 6) ;; Saturday
                (list (list (list 12 27 y) "Christmas Day + 2")))
              ((= d 0) ;; Sunday
                (list (list (list 12 26 y) "Christmas Day + 1")))
              (t
                (list (list (list 12 25 y) "Christmas Day"))))) )))))

(defun lorg-holiday-float (month dayname n string &optional day)
  (cond
    ((eq lorg-calendar-style 'twelve-months)
      (let* ((d
               (or
                  day
                  (if (> n 0)
                    1
                    (calendar-last-day-of-month month (lorg-target-year-function month))))))
        (list (list (calendar-nth-named-day n dayname month (lorg-target-year-function month) d) string))))
    ((eq lorg-calendar-style 'three-months)
      (let* ((m1 displayed-month)
             (y1 displayed-year)
             (m2 displayed-month)
             (y2 displayed-year)
             (d1 (progn             ; first possible base date for holiday
                   (calendar-increment-month m1 y1 -1)
                   (+ (calendar-nth-named-absday 1 dayname m1 y1)
                      (* -7 n)
                      (if (> n 0) 1 -7))))
             (d2                     ; last possible base date for holiday
              (progn
                (calendar-increment-month m2 y2 1)
                (+ (calendar-nth-named-absday -1 dayname m2 y2)
                   (* -7 n)
                   (if (> n 0) 7 -1))))
             (y1 (calendar-extract-year (calendar-gregorian-from-absolute d1)))
             (y2 (calendar-extract-year (calendar-gregorian-from-absolute d2)))
             (y                             ; year of base date
              (if (or (= y1 y2) (> month 9))
                  y1
                y2))
             (d                             ; day of base date
              (or day (if (> n 0)
                          1
                        (calendar-last-day-of-month month y))))
             (date                        ; base date for holiday
              (calendar-absolute-from-gregorian (list month d y))))
        (and (<= d1 date) (<= date d2)
             (list (list (calendar-nth-named-day n dayname month y d)
                         string)))))))

(defun lorg-filter-visible-calendar (hlist)
  "Filter list of holidays HLIST, and return only the visible ones.
HLIST is a list of elements of the form (DATE) TEXT."
  (delq nil (mapcar (lambda (p)
                      (and (car p) (lorg-calendar-date-is-visible-p (car p)) p))
                    hlist)))

(defun lorg-cal-sexp (sexp string)
  (let* (
      (m displayed-month)
      (y displayed-year)
      year
      date)
    (calendar-increment-month m y -1)
    (lorg-filter-visible-calendar
     (list
      (progn
        (setq year y
              date (eval sexp))
        (list date (if date (eval string)))) ))))

(defun lorg-calendar-goto-today ()
  "Reposition the lorg-calendar window so the current date is visible."
  (interactive)
  (let* (
      (today (lorg-calendar-current-date))
      (month (calendar-extract-month today))
      (day (calendar-extract-day today))
      (year (calendar-extract-year today)))
    (if (not (lorg-calendar-date-is-visible-p today))
      (progn
        (lorg-calendar-generate month year)
        (lorg-calendar-cursor-to-visible-date today))
      (calendar-update-mode-line)
      (lorg-calendar-cursor-to-visible-date today))))

(defun lorg-calendar-forward-month (arg)
(interactive "p")
  (let* (
      (arg (if arg arg 1))
      month
      year)
    (with-current-buffer (get-buffer lorg-calendar-buffer)
      (setq month displayed-month)
      (setq year displayed-year)
      (calendar-increment-month month year arg)
      (lorg-calendar-generate month year))))

(defun lorg-calendar-forward-year (arg)
  "Move the cursor forward by ARG years.  Movement is backward if ARG is negative."
  (interactive "p")
  (lorg-calendar-forward-month (* 12 arg)))

(defun lorg-calendar-backward-month (arg)
  "Move the cursor backward by ARG months.  Movement is forward if ARG is negative."
  (interactive "p")
  (lorg-calendar-forward-month (- arg)))

(defun lorg-calendar-backward-year (arg)
  "Move the cursor backward ARG years. Movement is forward is ARG is negative."
  (interactive "p")
  (lorg-calendar-forward-month (* -12 arg)))

(defun lorg-calendar-current-date (&optional offset)
  "Return the current date in a list (month day year).
Optional integer OFFSET is a number of days from the current date."
  (let* ((now (decode-time))
         (now (list (nth 4 now) (nth 3 now) (nth 5 now))))
    (if (zerop (or offset 0))
        now
      (calendar-gregorian-from-absolute
       (+ offset (calendar-absolute-from-gregorian now))))))

(defun lorg-message (input)
(interactive)
  (message
    (propertize input 'face 'font-lock-warning-face)))

(defvar lorg-court-holiday-face (make-face 'lorg-court-holiday-face))
(set-face-attribute 'lorg-court-holiday-face nil
  :background "chocolate4" :foreground "black")

(defvar lorg-birthday-face (make-face 'lorg-birthday-face))
(set-face-attribute 'lorg-birthday-face nil
  :background "firebrick" :foreground "black")

(defvar lorg-birthday+others-face (make-face 'lorg-birthday+others-face))
(set-face-attribute 'lorg-birthday+others-face nil
  :background nil :foreground nil :box '(:line-width 2 :color "red"))

(defvar lorg-mouse-calendar-face (make-face 'lorg-mouse-calendar-face))
(set-face-attribute 'lorg-mouse-calendar-face nil
  :background "LightCoral" :foreground "black")

(defvar lorg-court-appearance-face (make-face 'lorg-court-appearance-face))
(set-face-attribute 'lorg-court-appearance-face nil
  :background "chartreuse" :foreground "black")

(defvar lorg-appointment-face (make-face 'lorg-appointment-face))
(set-face-attribute 'lorg-appointment-face nil
  :background "cyan" :foreground "black")

(defvar lorg-appointment-appearance-face (make-face 'lorg-appointment-appearance-face))
(set-face-attribute 'lorg-appointment-appearance-face nil
  :background "orange" :foreground "black")

(defvar lorg-holiday-appointment-face (make-face 'lorg-holiday-appointment-face))
(set-face-attribute 'lorg-holiday-appointment-face nil
  :background "yellow" :foreground "black")

(defun lorg-narrow-p ()
  "Return t if a buffer is narrowed"
  (not (equal (- (point-max) (point-min)) (buffer-size))))

(defun lorg-display-buffer-below (buffer alist)
 (let (
    (window
      (cond
        ((get-buffer-window buffer (selected-frame))
          (get-buffer-window buffer (selected-frame)))
        ((window-in-direction 'below)
          (window-in-direction 'below))
        (t
          (split-window (selected-window) nil 'below)))))
  (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)))

(defun lorg-display-buffer-left (buffer alist)
"(1) If `buffer` is already displayed, then display it again in the same window.
(2) If `buffer` is not already displayed, and if there is a window to the left,
then display that `buffer` in said window. (3) If `buffer` is not already
displayed, and if there is a window to the right, then use the selected window.
(4) If all else fails, then create a new window to the left and display `buffer` there."
 (let (
    (window
      (cond
        ((get-buffer-window buffer (selected-frame))
          (get-buffer-window buffer (selected-frame)))
        ((window-in-direction 'above)
          (window-in-direction 'above))
        ((window-in-direction 'left)
          (window-in-direction 'left))
        ((window-in-direction 'right)
          (selected-window))
        (t
          (split-window (selected-window) nil 'left)))))
  (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)))

(defun lorg-display-buffer-right (buffer alist)
"(1) If `buffer` is already displayed, then display it again in the same window.
(2) If `buffer` is not already displayed, and if there is a window to the right,
then display that `buffer` in said window. (3) If `buffer` is not already
displayed, and if there is a window to the left, then use the selected window.
(4) If all else fails, then create a new window to the right and display `buffer` there."
 (let (
    (window
      (cond
        ((get-buffer-window buffer (selected-frame))
          (get-buffer-window buffer (selected-frame)))
        ((window-in-direction 'above)
          (window-in-direction 'above))
        ((window-in-direction 'right)
          (window-in-direction 'right))
        ((window-in-direction 'left)
          (selected-window))
        (t
          (split-window (selected-window) nil 'right)))))
  (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)))

(defun lorg-calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (or (lorg-calendar-cursor-to-date)
      (let* ((col (current-column))
             (edges (cdr (assoc (calendar-column-to-segment)
                                calendar-month-edges)))
             (last (nth 2 edges))
             (right (nth 3 edges)))
        (when (< (count-lines (point-min) (point)) calendar-first-date-row)
          (goto-char (point-min))
          (forward-line (1- calendar-first-date-row))
          (move-to-column col))
        ;; The date positions are fixed and computable, but searching
        ;; is probably more flexible.  Need to consider blank days at
        ;; start and end of month if computing positions.
        ;; 'date text-property is used to exclude intermonth text.
        (unless (and (looking-at "[0-9]")
                     (get-text-property (point) 'date))
          ;; We search forwards for a number, except close to the RH
          ;; margin of a month, where we search backwards.
          ;; Note that the searches can go to other lines.
          (if (or (looking-at " *$")
                  (and (> col last) (< col right)))
              (while (and (re-search-backward "[0-9]" nil t)
                          (not (get-text-property (point) 'date))))
            (while (and (re-search-forward "[0-9]" nil t)
                        (not (get-text-property (1- (point)) 'date))))
            (backward-char 1)))
        (lorg-calendar-cursor-to-date))))

(defmacro lorg-calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive.  The standard macro `dotimes' is preferable in most cases."
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

(defun lorg-calendar-goto-today ()
  "Reposition the lorg-calendar window so the current date is visible."
  (interactive)
  (let* (
      (today (calendar-current-date))
      (month (calendar-extract-month today))
      (day (calendar-extract-day today))
      (year (calendar-extract-year today)))
    (if (not (lorg-calendar-date-is-visible-p today))
      (progn
        (lorg-calendar-generate month year)
        (lorg-calendar-cursor-to-visible-date today))
      (calendar-update-mode-line)
      (lorg-calendar-cursor-to-visible-date today))))

(provide 'lorg-calendar)
