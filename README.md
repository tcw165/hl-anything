[![MELPA](http://melpa.org/packages/hl-anything-badge.svg)](http://melpa.org/#/hl-anything)
[![MELPA Stable](http://stable.melpa.org/packages/hl-anything-badge.svg)](http://stable.melpa.org/#/hl-anything)
[![TRAVIS-CI](https://travis-ci.org/boyw165/hl-anything.svg?branch=master)](https://travis-ci.org/boyw165/hl-anything)

hl-anything.el - Highlight Anything in Emacs
============================================

Highlight things in a text file makes you search things easily. It is fundamental and very helpful to everyone, enjoy!

Highlight symbols with different colors.
----------------------------------------
> Note: The highlights are still visible even under current line highlight (`hl-line-mode` or `global-hl-line-mode`).
![hl-anything screenshot](demo/symbol-hl.gif "hl-anything demo")

Highlight selections with different colors.
-------------------------------------------
> Note: The selections can span across spaces or line break.
![hl-anything screenshot](demo/selection-hl.gif "hl-anything demo")

Highlight things in a highlighted thing.
----------------------------------------
![hl-anything screenshot](demo/things-in-things.gif "hl-anything demo")

Highlight enclosing inward and outward parentheses.
---------------------------------------------------
![hl-anything screenshot](demo/enclosing-paren.png "hl-anything demo")

Select highlighted things smartly and search forwardly or backwardly.
---------------------------------------------------------------------
![hl-anything screenshot](demo/search.gif "hl-anything demo")

Specify faces to be visible under current line highlight.
---------------------------------------------------------
![hl-anything screenshot](demo/face-hl.png "hl-anything demo")

Usage
-----
Add the following code to your `.emacs` file:
``` lisp
(require 'hl-anything)
(hl-highlight-mode 1)
```

Useful Functions:
* Toggle highlight globally in all buffers: `M-x hl-highlight-thingatpt-global`
* Remove all highlights in all buffers:  or `M-x hl-unhighlight-all-global`
* Toggle highlight locally in current buffer (every buffers have its own copy): `M-x hl-highlight-thingatpt-local`
* Remove all highlights in current buffer: `M-x hl-unhighlight-all-local`
* Search highlights: `M-x hl-find-thing-forwardly` or `M-x hl-find-thing-backwardly`
* Save highlights both of local and global highlights: `M-x hl-save-highlights`
* Restore highlights both of local and global highlights: `M-x hl-restore-highlights`
* Enable parenethese highlight: `M-x hl-paren-mode`

Customization:
* Change highlight colors: `M-x customize-group` Enter `hl-anything`.
* `hl-highlight-foreground-colors` and `hl-highlight-background-colors` are foreground and background colors setting.
* `hl-highlight-save-file` is a file storing highlights; `hl-auto-save-restore-highlights` is to automatically save and restore highlights.

TODO
----
* Highlight Enclosing syntax in Emacs REGEX.

Contribution
------------
Forks and pull requests are welcome!

Lincense
--------
The MIT License (MIT)

Copyright (c) 2014-2015 boyw165

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
