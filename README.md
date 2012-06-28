emacs-google-this
=================

A set of emacs functions and bindings to google under point.

Overview
====

*google-this.el* is a package that provides a set of functions and keybindings for
launching google searches from within emacs.

The main function is `google-this` (bound to C-x g t). It does a google search
using the currently selected region, or the expression under point. All
functions are bound under "C-x g".

To start a blank search, do `google-search` (C-x g RET). If you want more
control of what "under point" means, there are the `google-word`,
`google-symbol`, `google-line` and `google-region`functions, bound as w, s, l
and r, respectively.

If the `google-wrap-in-quotes` variable is t, than searches are enclosed by
double quotes (default is NOT). If a prefix argument is given to any of the
functions, invert the effect of `google-wrap-in-quotes`.

INSTALLATION
===

Make sure "google-this.el" is in your load path, then place this code in your
.emacs file:

	(require 'google-this)
	(google-this-mode 1)

