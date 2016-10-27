lorg-calendar.el
================

    INSTRUCTIONS:  M-x lorg-calendar-generate

    To scroll month by month forward, use:  >
    To scroll month by month backward, use:  <
    To mark a new date not yet selected, use the left click on the mouse,
      which will also copy the date to the kill-ring.  Left-clicking on a
      date previously marked with the left-click will remove the overlay.

`lorg-calendar` is a simplified variation of a custom calendar that I use daily, which is on Github just in case anyone is interested in seeing how it works.  It does not come with any instructions other than the first few lines in the comments at the beginning of the file (same as above).  It is an Emacs 3-month or 12-month calendar that scrolls (forwards/backwards in time) and is able to mark birthdays, holidays, appointments, court appearances, and dates previously marked with the mouse.

The 12-month scrolling calendar is a culmination of help that I received from several people who responded to various threads on stackoverflow.com regarding algorithms and so forth.  Where help was provided, I have included citations in the comments of the code.  It relies upon many of the existing functions/features of the Emacs built-in `calendar` and `org-mode` libraries.

The example of calendared dates (birthdays, appointments and court appearances) are in the month of **February 2014**.  The pre-programmed holidays (that automatically appear for all years) are the ones recognized by the Los Angeles Superior Court in California.

In my own setup (not available to the public) I use a custom modified version of `calfw`, which shows 1 month, 2 weeks, 1 week, or 1 day.  If I want to see 3 months or 12 months, then I use a more complex version of `lorg-calendar`, which is able to generate agenda views when I click on a date.

I synchronize my tasks/events with Toodledo over the internet with a custom modified version of `org-toodledo` (not available to the public), which supports all keywords and priorities (plus an extra field or two such as location); and, that is why the tasks/events in the example are in a specific format.

Here is a link to an even more simplified version of the 12-month scrolling calendar that I posted on stackoverflow.com:  http://stackoverflow.com/a/21409154/2112489

![screenshot](http://www.lawlist.com/images/calendar_example.png)