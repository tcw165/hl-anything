============================================
hl-anything.el - Highlight Anything in Emacs
============================================

![hl-anything screenshot](demo/basic.gif "hl-anything demo")

What is it?
-----------
Things highlight in a text file is fundamental and very helpful to everyone.

Symbols highlights with different colors. Note: The highlights are still visible even under current line highlight (`hl-line-mode` or `global-hl-line-mode`).
![hl-anything screenshot](demo/symbol-hl.gif "hl-anything demo")

---
Selections highlights with different colors. Note: The selections can span cross spaces or line break.
![hl-anything screenshot](demo/selection-hl.gif "hl-anything demo")

---
Highlight things in a highlighted thing.
![hl-anything screenshot](demo/things-in-things.gif "hl-anything demo")

---
Highlight enclosing inward and outward parentheses.
![hl-anything screenshot](demo/enclosing-paren.png "hl-anything demo")

---
Smartly select highlighted things and search forwardly or backwardly.
![hl-anything screenshot](demo/search.gif "hl-anything demo")

---
Specify faces to be visible under current line highlight.
![hl-anything screenshot](demo/face-hl.png "hl-anything demo")

Usage
-----
Add the following code to your `.emacs` file:
```cl
(require 'hl-anything)
```

Interactive Functions:
* Toggle highlighting things at point: `M-x hl-highlight-thingatpt-local`
* Remove all highlights: `M-x hl-unhighlight-all-local`
* Search highlights: `M-x hl-find-thing-forwardly` or `M-x hl-find-thing-backwardly`
* Enable parenethese highlighting: `M-x hl-paren-mode`

TODO
----
* Support global highlights (which is highlights will appears in every buffers).
* Save highlights before Emacs closed and restore them after Emacs opened next time.

Lincense
--------
MIT
