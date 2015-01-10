[![MELPA](http://melpa.org/packages/hl-anything-badge.svg)](http://melpa.org/#/hl-anything)
[![MELPA Stable](http://stable.melpa.org/packages/hl-anything-badge.svg)](http://stable.melpa.org/#/hl-anything)
[![TRAVIS-CI](https://travis-ci.org/boyw165/hl-anything.svg?branch=master)](https://travis-ci.org/boyw165/hl-anything)

Highlight Symbols, Selections, Enclosing Parens and More
===================================================================

Highlight things at point, selections, enclosing parentheses with different colors. Fix grumbling issue of highlights being overridden by `hl-line-mode` and `global-hl-line-mode`.

Demo
----
![hl-anything screenshot](demo/all.gif "hl-anything demo")

Basic Usage - First Step, Enable `hl-highlight-mode`
----------------------------------------------------
There're **GLOBAL** and **LOCAL** kinds of highlights.
The global highlights appear in every buffer; The local highlights only exist in the current buffer.

Global Highlights:

* Command **`hl-highlight-thingatpt-global`** to toggle global highlight at point or selection.
* Command **`hl-unhighlight-all-global`** to remove all global highlights.

Local Highlights:

* Command **`hl-highlight-thingatpt-local`** to toggle local highlight at point or selection.
* Command **`hl-unhighlight-all-local`** to remove all local highlights.

You can change foreground and background colors of highlights:

* Foreground colors are stored in `hl-highlight-foreground-colors` variable.
* Background colors are stored in `hl-highlight-background-colors` variable.

![hl-anything screenshot](demo/highlight-colors.png "colors")

Search Highlights
-----------------
Put your cursor on the highlight and use following commands to search highlights.

* Command **`hl-find-thing-forwardly`** and **`hl-find-thing-backwardly`** to go through highlights.

![hl-anything screenshot](demo/highlight-search.gif "search")

Save & Restore Highlights
-------------------------
Once `hl-highlight-mode` is on, it will save highlights automatically before Emacs exits.
It will also try to restore highlights when Emacs opens.

* Command **`hl-save-highlights`** to save highlights; **`hl-restore-highlights`** to restore highlights.
* Saved file-path is stored in `hl-highlight-save-file` variable.

Parenthese Highlight, `hl-paren-mode`
-------------------------------------
![hl-anything screenshot](demo/highlight-paren.png "hl-paren-mode")

* Colors of outward parentheses are stored in `hl-outward-paren-fg-colors` and `hl-outward-paren-bg-colors` variables.
* Colors of inward parentheses are stored in `hl-inward-paren-fg-color` variables.

Advanced
--------
There's a special faces storage, `hl-highlight-special-faces`. The faces here are always on the top of the current line highlight (`hl-line-mode` and `global-hl-line-mode`).

![hl-anything screenshot](demo/highlight-special-faces.png "hl-paren-mode")

Example:

```lisp
(add-to-list 'hl-highlight-special-faces 'YOUR-FACE t)
```

TODO
----
* Add menu items and tool-bar buttons.
* Highlight Enclosing syntax in Emacs REGEX.

Contribution
------------
Forks and pull requests are welcome!

