lorg-calendar.el
================

3-month or 12-month scrolling calendar, with support for appointments, court appearances, birthdays, court holidays (Los Angeles Superior Court), and dates previously marked with the mouse.

    INSTRUCTIONS: M-x lorg-calendar-generate

    To scroll month by month forward, use:  >
    To scroll month by month backward, use:  <
    To mark a new date not yet selected, use the left click on the mouse, which will also copy the date to the kill-ring.  Left-clicking on a date previously marked with the left-click will remove the overlay.

`lorg-calendar` is a simplified variation of a custom calendar that I use daily, which is on Github just in case anyone is interested in seeing how it works.  It doesn't come with any instructions other than the first few lines in the comments at the beginning of the file (same as above).  It is an Emacs 12-month calendar that scrolls (forwards/backwards in time) and is able to mark birthdays, holidays and appointments.

The 12-month rotating calendar is a culmination of help that I received from several people who responded to various threads on stackoverflow.com regarding algorithms and so forth.  Where help was provided, I have included citations in the comments of the code.  It relies upon many of the existing functions/features of the Emacs built-in `calendar` and `org-mode` libraries.

The pre-programmed holidays are the ones recognized by the Los Angeles Superior Court in California.

I also use a custom modified version of `calfw`, which shows 1 month, 2 weeks, 1 week, or 1 day.  If I want to see 3 months or 12 months, then I use my own.

I synchronize my tasks/events with Toodledo over the internet with a custom modified version of `org-toodledo`, which supports all keywords and priorities; and, that is why the tasks/events in the example are in a specific format.

Here is a link to an even more simplified version of the 12-month rotating calendar that I posted on stackoverflow.com:

http://stackoverflow.com/a/21409154/2112489

![screenshot](http://www.lawlist.com/images/calendar_example.png)