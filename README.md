Commentary
==========

As the Eternal Holy War of tabs-versus-spaces rages on, even within
individual projects, an emacs minor mode arises from the depths of
github with the goal of easing the burden placed on the programmer
of trying to maintain consistency in text files.

If you jump into a file that uses tabs for indentation, you shall
continue using tabs for indentation. If you jump into a file that
uses spaces for indentation, you shall continue using spaces for
indentation. That's the idea.

Installation
============

* Put indent-hints.el in your load path
* Add the following line to your .emacs file:

    `(require 'indent-hints)`

You can then activate indent-hints mode in a buffer the usual way:
    `(indent-hints-mode)`

You can also use the globalized minor mode to enable
indent-hints-mode in all buffers:
    `(indent-hints-global-mode)`

Use
===

Look at your mode-line to see whether the buffer you're visiting is
space-loving or tab-loving.

TODO
====

* Add some helper functions to switch between tabs/spaces profiles.
* Actually disable the mode (clean up the minor-mode-alist) on disable.


