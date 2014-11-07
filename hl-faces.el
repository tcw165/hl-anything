;; Copyright (C) 2014
;;
;; Author: BoyW165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'hl-anything)
;;
;; Toggle highlighting things at point:
;;   M-x hl-highlight-thingatpt-local
;;
;; Remove all highlights:
;;   M-x hl-unhighlight-all-local
;;
;; Enable parenethese highlighting:
;;   M-x hl-paren-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-09-25 (0.0.5)
;;    1. Highlights won't be blocked behind the current line when `hl-line-mode'
;;       is enabled.
;;    2. Smartly select highlighted region.
;;    3. Highlight words across multiple lines.
;;
;; 2014-05-25 (0.0.4)
;;    Support searching thing. The regexp might be a symbol text or a selection text.
;;
;; 2014-05-20 (0.0.3)
;;    Support one inward parentheses highlight.
;;
;; 2014-05-19 (0.0.2)
;;    Support multiple outward parentheses highlight.
;;
;; 2014-05-16 (0.0.1)
;;    Initial release, fork from http://nschum.de/src/emacs/highlight-parentheses.

(defface hl-file-face
  '((t (:foreground "blue" :underline t :weight bold)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-number-face
  '((t (:foreground "maroon1")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-generic-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-local-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-global-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-function-parameter-face
  '((t (:underline t)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-symbol-face
  '((t (:background "gold" :foreground "black" :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-title-1-face
  '((t (:background "LightCyan3" :foreground "gray40" :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-title-2-face
  '((t (:background "LightCyan2" :foreground "gray40" :weight bold :height 1.3)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(defface hl-title-3-face
  '((t (:background "LightCyan1" :foreground "gray40" :weight bold :height 1.1)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything-group)

(provide 'hl-faces)
