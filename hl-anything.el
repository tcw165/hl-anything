;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 24.3+
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
;; Main Features:
;; 1. Words or selections highlights with different colors set. The highlights
;;    are still visible even under current line highlight (`hl-line-mode' or 
;;    `global-hl-line-mode' is enabled).
;; 2. Search highlighted things at point in the current buffer.
;; 3. Highlight outward and inward parentheses with different colors set.
;;
;; Add the following to your .emacs file:
;; (require 'hl-anything)
;;
;; Toggle highlighting things at point:
;;   M-x `hl-highlight-thingatpt-local'
;;
;; Remove all highlights:
;;   M-x `hl-unhighlight-all-local'
;;
;; Search highlights:
;;   M-x `hl-find-thing-forwardly'
;;   M-x `hl-find-thing-backwardly'
;;
;; Enable parenethese highlighting:
;;   M-x `hl-paren-mode'
;;
;; Extended Feature:
;; 1. Additional faces set for temporary highlights.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TODO:
;; - Implement `hl-highlight-thingatpt-global'.
;; - Save highlights before Emacs closed in order to restore them after Emacs
;;   opened?
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-10-03 (0.0.6)
;;    - Support highlight for special faces. See `hl-highlight-special-faces'.
;;
;; 2014-09-25 (0.0.5)
;;    - Highlights are still visible under the current line when `hl-line-mode'
;;      or `global-hl-line-mode' is enabled.
;;    - Smartly select highlighted region.
;;    - Highlight words cross multiple lines.
;;
;; 2014-05-25 (0.0.4)
;;    - Support searching thing. The regexp might be a symbol text or a selection
;;      text.
;;
;; 2014-05-20 (0.0.3)
;;    - Support one inward parentheses highlight for LISP.
;;
;; 2014-05-19 (0.0.2)
;;    - Support multiple outward parentheses highlight for LISP.
;;
;; 2014-05-16 (0.0.1)
;;    - Initial release, fork from http://nschum.de/src/emacs/highlight-parentheses.

;; GNU Library.
(require 'thingatpt)

;; 3rd Party Library.
(require 'hl-faces)

(defgroup hl-anything-group nil
  "Highlight anything."
  :tag "hl-anything"
  :group 'faces
  :group 'font-lock
  :group 'matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom hl-fg-colors '("snow"
                          "snow"
                          "black"
                          "black"
                          "snow"
                          "snow"
                          "snow"
                          "black"
                          "snow"
                          "snow")
  "The foreground colors for `hl-highlight-thingatpt'."
  :type '(repeat color)
  :tag "Highlight Foreground Colors"
  :group 'hl-anything-group)

(defcustom hl-bg-colors '("firebrick"
                          "Orange"
                          "gold"
                          "green1"
                          "DeepSkyBlue1"
                          "dark blue"
                          "blue violet"
                          "gray90"
                          "gray60"
                          "gray30")
  "The background colors for `hl-highlight-thingatpt'."
  :type '(repeat color)
  :tag "Highlight Background Colors"
  :group 'hl-anything-group)

(defcustom hl-before-find-thing-hook nil
  "Hook for doing something before `hl--thing-find' do the searching.
This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature."
  :type '(repeat function)
  :group 'hl-anything-group)

(defcustom hl-after-find-thing-hook nil
  "Hook for doing something after `hl--thing-find' do the searching.
This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature."
  :type '(repeat function)
  :group 'hl-anything-group)

(defcustom hl-highlight-special-faces '(hl-symbol-face
                                        hl-title-1-face
                                        hl-title-2-face
                                        hl-title-3-face)
  "For the faces that will be treat as highlights, which means overlays 
will also be created for these faces under current line."
  :type '(repeat face)
  :group 'hl-anything-group)

(defvar hl-timer nil)

(defvar hl-index 0)

(defvar hl-things-global nil
  "A global things list. Format: ((REGEXP . FACESPEC) ...)")

(defvar hl-index-local 0)
(make-variable-buffer-local 'hl-index-local)

(defvar hl-things-local nil
  "A local things list. Format: (REGEXP1 REGEXP2 ...)")
(make-variable-buffer-local 'hl-things-local)

(defvar hl-temp-keywords nil
  "A local keywords list. See `font-lock-keywords' for its format.")
(make-variable-buffer-local 'hl-temp-keywords)

(defvar hl-overlays-local nil
  "Overlays for highlighted things. Prevent them to being hide by 
`hl-line-mode'.")
(make-variable-buffer-local 'hl-overlays-local)

(defvar hl-is-highlight-special-faces nil
  "Force to create `hl-overlays-local' overlays.")
(make-variable-buffer-local 'hl-is-highlight-special-faces)

(defun hl-thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string
 of selection."
  (let ((bound (if mark-active
                   (cons (region-beginning) (region-end))
                 (hl-bounds-of-thingatpt))))
    (when bound
      (let ((text (regexp-quote
                   (buffer-substring-no-properties (car bound) (cdr bound)))))
        ;; Replace space as "\\s-+"
        (setq text (replace-regexp-in-string "\\s-+" "\\\\s-+" text))
        (list text (car bound) (cdr bound))))))

(defun hl-bounds-of-thingatpt ()
  (or (hl-bounds-of-highlight)
      (bounds-of-thing-at-point 'symbol)))

(defun hl-bounds-of-highlight ()
  "Return the start and end locations for the highlighted things at point.
Format: (START . END)"
  (let ((face (hl-get-text-valid-face)))
    (when face
      (let (beg end)
        ;; Find beginning locations.
        (save-excursion
          (hl-bounds-of-valid-face face -1)
          (setq beg (point)))
        ;; Find end locations.
        (save-excursion
          (hl-bounds-of-valid-face face 1)
          (setq end (1+ (point))))
        (cons beg end)))))

(defun hl-get-text-valid-face ()
  (let ((face (get-text-property (point) 'face)))
    (cond
     ;; NULL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null face)
      nil)
     ;; Normal Face ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((facep face)
      (and (memq face hl-highlight-special-faces)
           face))
     ;; Face List ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((facep (car face))
      (setq face (car face))
      (and (memq face hl-highlight-special-faces)
           face))
     ;; Foreground-color & Background-color ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (t
      (let (elm ret)
        (when (setq elm (assoc 'foreground-color face))
          (setq ret (append ret `(,elm))))
        (when (setq elm (assoc 'background-color face))
          (setq ret (append ret `(,elm))))
        ret)))))

(defun hl-bounds-of-valid-face (org-face step)
  (when (/= step 0)
    (let ((face (hl-get-text-valid-face)))
      (ignore-errors
        (if (equal org-face face)
            (progn
              (forward-char step)
              (hl-bounds-of-valid-face org-face step))
          (backward-char step))))))

(defun hl-highlight-internal (regexp &optional local)
  (let* ((fg (nth hl-index-local hl-fg-colors))
         (bg (nth hl-index-local hl-bg-colors))
         (max (max (length hl-fg-colors)
                   (length hl-bg-colors)))
         (next-index (1+ hl-index-local))
         facespec)
    (push regexp hl-things-local)
    (setq hl-index-local (if (>= next-index max) 0 next-index))
    ;; Highlight.
    (when fg
      (setq facespec (append facespec `((foreground-color . ,fg)))))
    (when bg
      (setq facespec (append facespec `((background-color . ,bg)))))
    (font-lock-add-keywords nil `((,regexp 0 ',facespec prepend)) 'append)
    (font-lock-fontify-buffer)))

(defun hl-unhighlight-internal (regexp &optional local)
  (let* ((keyword (hl-is-font-lock-keywords regexp)))
    (setq hl-things-local (delete regexp hl-things-local))
    (unless hl-things-local
      (setq hl-index-local 0))
    ;; Unhighlight.
    (while (setq keyword (hl-is-font-lock-keywords regexp))
      (font-lock-remove-keywords nil `(,keyword)))
    (font-lock-fontify-buffer)))

(defun hl-is-font-lock-keywords (regexp)
  (assoc regexp (if (eq t (car font-lock-keywords))
                    (cadr font-lock-keywords)
                  font-lock-keywords)))

(defun hl-highlight-pre-command ()
  ;; Remove temporarily keywords.
  (when hl-temp-keywords
    (font-lock-remove-keywords nil hl-temp-keywords)
    (font-lock-fontify-buffer)
    (setq hl-temp-keywords nil))
  (when hl-timer
    (cancel-timer hl-timer)
    (setq hl-timer nil)))

(defun hl-highlight-post-command ()
  (when (hl-is-begin)
    (setq hl-timer (run-with-idle-timer 0 nil 'hl-add-highlight-overlays))))

(defun hl-is-begin ()
  (not (or (active-minibuffer-window))))

(defun hl-add-highlight-overlays ()
  (when (or (and hl-highlight-mode
                 (require 'hl-line) hl-line-mode
                 (or hl-things-global hl-things-local hl-overlays-local
                     hl-temp-keywords))
            hl-is-highlight-special-faces)
    ;; Remove overlays.
    (mapc 'delete-overlay hl-overlays-local)
    (setq hl-overlays-local nil)
    ;; Create overlays.
    (let ((end (line-end-position))
          bound)
      (save-excursion
        (beginning-of-line)
        (while (<= (point) end)
          (if (setq bound (hl-bounds-of-highlight))
              (let ((overlay (make-overlay (point) (cdr bound)))
                    (face (hl-get-text-valid-face)))
                (if (facep face)
                    (let ((fg (face-attribute face :foreground))
                          (bg (face-attribute face :background))
                          facespec)
                      (when fg
                        (setq facespec
                              (append facespec `((foreground-color . ,fg)))))
                      (when bg
                        (setq facespec
                              (append facespec `((background-color . ,bg)))))
                      (overlay-put overlay 'face facespec))
                  (overlay-put overlay 'face face))
                (push overlay hl-overlays-local)
                (goto-char (cdr bound)))
            (forward-char)))))))

;;;###autoload
(defun hl-highlight-thingatpt-global ()
  "Toggle highlighting globally."
  (interactive)
  ;; TODO:
  )

;;;###autoload
(defun hl-highlight-thingatpt-local ()
  "Toggle highlighting locally in the current buffer."
  (interactive)
  (unless hl-highlight-mode
    (hl-highlight-mode 1))
  (let* ((thing (hl-thingatpt))
         (regexp (car thing)))
    (when thing
      (if (member regexp hl-things-local)
          (hl-unhighlight-internal regexp t)
        (hl-highlight-internal regexp t)))))

;;;###autoload
(defun hl-highlight-keywords-temporarily (keywords)
  "Highlight keywords locally and temporarily in the current buffer. Any action
 will remove the temporary highlights."
  (when keywords
    (setq hl-temp-keywords keywords)
    (font-lock-add-keywords nil keywords 'append)
    (font-lock-fontify-buffer)
    (unless hl-highlight-mode
      (hl-highlight-mode 1))))

;;;###autoload
(defun hl-unhighlight-all-local ()
  "Remove all the highlights in buffer."
  (interactive)
  (dolist (regexp hl-things-local)
    (hl-unhighlight-internal regexp t))
  (setq hl-index-local 0))

;;;###autoload
(define-minor-mode hl-highlight-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " hl-highlight"
  (if hl-highlight-mode
      (progn
        (add-hook 'pre-command-hook 'hl-highlight-pre-command t t)
        (add-hook 'post-command-hook 'hl-highlight-post-command t t)
        (hl-add-highlight-overlays))
    (remove-hook 'pre-command-hook 'hl-highlight-pre-command t)
    (remove-hook 'post-command-hook 'hl-highlight-post-command t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select & Search Highlighted Things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hl-find-thing (step)
  (let* ((regexp (hl-thingatpt))
         (match (nth 0 regexp))
         (beg (nth 1 regexp))
         (end (nth 2 regexp))
         (case-fold-search t))
    (when regexp
      ;; Hook before searching.
      (run-hook-with-args hl-before-find-thing-hook regexp)
      (setq mark-active nil)
      (goto-char (nth (if (> step 0)
                          ;; Move to end.
                          2
                        ;; Move to beginning.
                        1) regexp))
      (if (re-search-forward match nil t step)
          (progn
            (set-marker (mark-marker) (match-beginning 0))
            (goto-char (match-end 0)))
        (set-marker (mark-marker) beg)
        (goto-char end))
      (setq mark-active t)
      ;; Hook after searching.
      (run-hook-with-args hl-after-find-thing-hook regexp))))

;;;###autoload
(defun hl-find-thing-forwardly ()
  "Find regexp forwardly and jump to it."
  (interactive)
  (hl-find-thing 1))

;;;###autoload
(defun hl-find-thing-backwardly ()
  "Find regexp backwardly and jump to it."
  (interactive)
  (hl-find-thing -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parentheses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hl-paren-custom-set (symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (set symbol value)
      (when hl-paren-mode
        (hl-paren-mode -1)
        (hl-paren-mode 1)))))

(defcustom hl-outward-paren-fg-colors '("black"
                                        "black")
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything-group)

(defcustom hl-outward-paren-bg-colors '("cyan"
                                        "gold")
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything-group)

(defcustom hl-inward-paren-fg-color "snow"
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type 'color
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything-group)

(defcustom hl-inward-paren-bg-color "magenta1"
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type 'color
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything-group)

(defface hl-paren-face nil
  "Face used for highlighting parentheses."
  :group 'hl-anything-group)

(defvar hl-paren-timer nil)

(defvar hl-outward-parens nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-outward-parens)

(defvar hl-inward-parens nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-inward-parens)

(defun hl-paren-idle-begin ()
  (when (hl-paren-is-begin)
    (setq hl-paren-timer (run-with-idle-timer 0.1 nil 'hl-create-parens))))

(defun hl-paren-is-begin ()
  (not (or (active-minibuffer-window))))

(defun hl-create-parens ()
  "Highlight the parentheses around point."
  (when hl-paren-mode
    (hl-create-parens-internal)
    ;; Outward overlays.
    (let ((overlays hl-outward-parens))
      (save-excursion
        (ignore-errors
          (while overlays
            (up-list -1)
            (move-overlay (pop overlays) (point) (1+ (point)))
            (forward-sexp)
            (move-overlay (pop overlays) (1- (point)) (point)))))
      ;; Hide unused overlays.
      (dolist (overlay overlays)
        (move-overlay overlay 1 1)))
    ;; Inward overlays.
    (unless (memq (get-text-property (point) 'face)
                  '(font-lock-comment-face
                    font-lock-string-face))
      (let ((overlays hl-inward-parens))
        (save-excursion
          (ignore-errors
            (cond
             ;; Open parenthesis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ((eq ?\( (char-syntax (char-after)))
              (move-overlay (pop overlays) (point) (1+ (point)))
              (forward-sexp)
              (move-overlay (pop overlays) (1- (point)) (point)))
             ;; Close parenthesis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ((eq ?\) (char-syntax (char-before)))
              (move-overlay (pop overlays) (1- (point)) (point))
              (backward-sexp)
              (move-overlay (pop overlays) (point) (1+ (point)))))))))))

(defun hl-create-parens-internal ()
  ;; outward overlays.
  (unless hl-outward-parens
    (let ((fg hl-outward-paren-fg-colors)
          (bg hl-outward-paren-bg-colors))
      (while (or fg bg)
        (let (facespec)
          (when fg
            (setq facespec (append facespec `((foreground-color . ,(car fg))))))
          (pop fg)
          (when bg
            (setq facespec (append facespec `((background-color . ,(car bg))))))
          (pop bg)
          ;; Make pair overlays.
          (dotimes (i 2)
            (push (make-overlay 0 0) hl-outward-parens)
            (overlay-put (car hl-outward-parens) 'face facespec))))
      (setq hl-outward-parens (reverse hl-outward-parens))))
  ;; inward overlays.
  (unless hl-inward-parens
    (let ((fg hl-inward-paren-fg-color)
          (bg hl-inward-paren-bg-color)
          facespec)
      (when fg
        (setq facespec (append facespec `((foreground-color . ,fg)))))
      (when bg
        (setq facespec (append facespec `((background-color . ,bg)))))
      ;; Make pair overlays.
      (dotimes (i 2)
        (push (make-overlay 0 0) hl-inward-parens)
        (overlay-put (car hl-inward-parens) 'face facespec)))))

(defun hl-remove-parens ()
  (when hl-paren-timer
    (cancel-timer hl-paren-timer)
    (setq hl-paren-timer nil))
  (mapc 'delete-overlay hl-outward-parens)
  (mapc 'delete-overlay hl-inward-parens)
  (mapc 'kill-local-variable '(hl-outward-parens
                               hl-inward-parens)))

;;;###autoload
(define-minor-mode hl-paren-mode
  "Minor mode to highlight the surrounding parentheses."
  :lighter " hl-paren"
  (if hl-paren-mode
      (progn
        (add-hook 'pre-command-hook 'hl-remove-parens nil t)
        (add-hook 'post-command-hook 'hl-paren-idle-begin nil t))
    (remove-hook 'pre-command-hook 'hl-remove-parens t)
    (remove-hook 'post-command-hook 'hl-paren-idle-begin t)))

(provide 'hl-anything)
