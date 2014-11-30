# What the hell is this? #

This is a simple Emacs major mode for programming IDP. Currently,
there's only very basic syntax highlighting (I'm new at creating Emacs
major modes and new at Emacs Lisp). Hopefully future features:

* more extensive and intelligent syntax highlighting (ex. highlighting
  user-defined types as types, showing other instances of a logical
  variable when the cursor is on one)

* replacing ASCII logical characters by nice Unicode symbols, like in the IDP IDE
using a minor mode! http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/
* correct indentation (this is a toughie)
* refactoring support
* in-buffer evaluation (like eval-print-last-sexp in Emacs Lisp buffers)
* syntax error checking
