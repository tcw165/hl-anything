;;; hl-anything.el --- Highlight symbols, selections, enclosing parens and more.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 20150109.2300
;; Package-Requires: ((emacs "24.3"))
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
;; Highlight things at point, selections, enclosing parentheses with different
;; colors. Fix grumbling issue of highlights being overridden by `hl-line-mode'
;; and `global-hl-line-mode'.
;;
;; Of course, there're more advanced features:
;; * Save highlights and restore them next time Emacs opened.
;; * Select highlighted things smartly and search forwardly or backwardly.
;; * More... Check official website for details:
;; https://github.com/boyw165/hl-anything
;;
;; Usage:
;; ------
;; M-x `hl-highlight-thingatpt-local'
;; Toggle highlight locally in current buffer.
;;
;; M-x `hl-highlight-thingatpt-global'
;; Toggle highlight globally in all buffers.
;;
;; M-x `hl-unhighlight-all-local'
;; M-x `hl-unhighlight-all-global'
;; Remove all highlights.
;;
;; M-x `hl-save-highlights'
;; M-x `hl-restore-highlights'
;; Save & Restore highlights.
;;
;; M-x `hl-find-thing-forwardly'
;; M-x `hl-find-thing-backwardly'
;; Search highlights.
;;
;; M-x `hl-paren-mode'
;; Enable enclosing parenethese highlighting.
;;
;; TODO:
;; -----
;; * Highlight enclosing syntax in REGEXP.
;; * Remove `hl-face', they seems redundant.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-11-24
;; * Rename `hl-fg-colors' to `hl-highlight-foreground-colors'.
;;          `hl-bg-colors' to `hl-highlight-background-colors'.
;;   Note: Users should update your codes refer to the patch!
;; * Support `hl-highlight-thingatpt-global' and `hl-unhighlight-all-global'.
;; * Support `hl-save-highlights' and `hl-restore-highlights'.
;;
;; 2014-10-03
;; * Support highlight for special faces. See `hl-highlight-special-faces'.
;; * Highlights are still visible under the current line when `hl-line-mode'
;;   or `global-hl-line-mode' is enabled.
;; * Smartly select highlighted region.
;; * Highlight words cross multiple lines.
;;
;; 2014-05-25
;; * Support searching thing. The regexp might be a symbol text or a selection
;;   text.
;; * Support one inward parentheses highlight for LISP.
;; * Support multiple outward parentheses highlight for LISP.
;;
;; 2014-05-16
;; * Initial release, inspired from:
;;   https://github.com/nschum/highlight-parentheses.el
;;   https://github.com/nschum/highlight-symbol.el
;;
;;; Code:

;; GNU Library.
(require 'hl-line)
(require 'thingatpt)

(defgroup hl-anything nil
  "Highlight anything."
  :tag "hl-anything"
  :group 'faces
  :group 'font-lock
  :group 'matching)

(defgroup hl-paren nil
  "Parentheses highlight."
  :tag "hl-paren"
  :group 'hl-anything)

(defgroup hl-face nil
  "Additional faces for `hl-anything'."
  :tag "hl-face"
  :group 'hl-anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface hl-file-face
  '((t (:foreground "blue" :underline t :weight bold)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-number-face
  '((t (:foreground "maroon1")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-generic-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-local-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-global-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-function-parameter-face
  '((t (:underline t)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-symbol-face
  '((t (:background "gold" :foreground "black" :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-title-1-face
  '((t (:background "LightCyan3" :foreground "gray40" :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-title-2-face
  '((t (:background "LightCyan2" :foreground "gray40" :weight bold :height 1.3)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

(defface hl-title-3-face
  '((t (:background "LightCyan1" :foreground "gray40" :weight bold :height 1.1)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom hl-highlight-foreground-colors '("snow"
                                            "snow"
                                            "black"
                                            "black"
                                            "snow"
                                            "snow"
                                            "snow"
                                            "black"
                                            "snow"
                                            "snow")
  "The foreground colors for `hl-highlight-thingatpt-global' and
`hl-highlight-thingatpt-local'."
  :type '(repeat color)
  :tag "Highlight Foreground Colors"
  :group 'hl-anything)

(defcustom hl-highlight-background-colors '("firebrick"
                                            "Orange"
                                            "gold"
                                            "green1"
                                            "DeepSkyBlue1"
                                            "dark blue"
                                            "blue violet"
                                            "gray90"
                                            "gray60"
                                            "gray30")
  "The background colors for `hl-highlight-thingatpt-global' and
`hl-highlight-thingatpt-local'."
  :type '(repeat color)
  :tag "Highlight Background Colors"
  :group 'hl-anything)

(defcustom hl-before-find-thing-hook nil
  "Hook for doing something before `hl-find-thing-forwardly' and 
`hl-find-thing-backwardly'. This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature."
  :type '(repeat function)
  :group 'hl-anything)

(defcustom hl-after-find-thing-hook nil
  "Hook for doing something after `hl-find-thing-forwardly' and 
`hl-find-thing-backwardly'. This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature."
  :type '(repeat function)
  :group 'hl-anything)

(defcustom hl-highlight-special-faces nil
  "For the faces that will be treat as highlights, which means overlays 
will also be created for these faces at current line."
  :type '(repeat face)
  :group 'hl-anything)

(defcustom hl-highlight-save-file "~/.emacs.d/.hl-save"
  "A file storing highlights. Call `hl-restore-highlights' to restore highlights.
See `hl-save-highlights' for detailed format."
  :type 'string
  :group 'hl-anything)

(defcustom hl-auto-save-restore-highlights t
  "TRUE to indicate storing highlights before killing Emacs and restore them next 
time. You can alos call `hl-restore-highlights' manually to restore highlights;
Or call `hl-save-highlights' to save highlights."
  :type 'boolean
  :group 'hl-anything)

(defvar hl-timer nil)

(defvar hl-region nil
  "A struct, (START . END), is present when `region-active-p' it t.")

(defvar hl-colors-index 0)

(defvar hl-colors-index-local 0)
(make-variable-buffer-local 'hl-colors-index-local)

(defvar hl-highlights nil
  "Highlights list. Format: (MATCH1 MATCH2 ...)")

(defvar hl-highlights-local nil
  "Local highlights list. Format: (MATCH1 MATCH2 ...)")
(make-variable-buffer-local 'hl-highlights-local)

(defvar hl-overlays nil
  "Overlays for highlighted things. Prevent them to being hide by `hl-line-mode'.")
(make-variable-buffer-local 'hl-overlays)

(defun hl-export (filename data)
  (and (file-writable-p filename)
       (with-temp-file filename
         (insert (let (print-length)
                   (prin1-to-string data))))))

(defun hl-import (filename)
  (and (file-exists-p filename)
       (with-temp-buffer
         (insert-file-contents-literally filename)
         (read (buffer-string)))))

(defun hl-thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string
 of selection."
  (let ((bound (if (use-region-p)
                   (cons (region-beginning) (region-end))
                 (hl-bounds-of-thingatpt))))
    (when bound
      (let ((text (regexp-quote
                   (buffer-substring-no-properties (car bound) (cdr bound)))))
        ;; Replace space as "\\s-+"
        (setq text (replace-regexp-in-string "\\s-+" "\\\\s-+" text))
        (list text (car bound) (cdr bound))))))

(defun hl-get-text-highlight-face ()
  (unless (and hl-region
               (>= (point) (car hl-region))
               (< (point) (cdr hl-region)))
    (let ((face (get-text-property (point) 'face)))
      (cond
       ;; NULL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((null face)
        nil)
       ;; Normal Face ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((facep face)
        (car (memq face hl-highlight-special-faces)))
       ;; Face List ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((facep (car face))
        (setq face (car face))
        (car (memq face hl-highlight-special-faces)))
       ;; Foreground-color & Background-color ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (t
        (let (elm ret)
          (when (setq elm (assoc 'foreground-color face))
            (setq ret (append ret `(,elm))))
          (when (setq elm (assoc 'background-color face))
            (setq ret (append ret `(,elm))))
          ret))))))

(defun hl-bounds-of-thingatpt ()
  (or (hl-bounds-of-highlight)
      (bounds-of-thing-at-point 'symbol)))

(defun hl-bounds-of-highlight ()
  "Return the start and end locations for the highlighted things at point.
Format: (START . END)"
  (let ((face (hl-get-text-highlight-face)))
    (when face
      (let (beg end)
        ;; Find beginning locations.
        (save-excursion
          (hl-seek-matched-face face -1)
          (setq beg (point)))
        ;; Find end locations.
        (save-excursion
          (hl-seek-matched-face face 1)
          (setq end (1+ (point))))
        (cons beg end)))))

(defun hl-seek-matched-face (matched-face step)
  (when (/= step 0)
    (let ((face (hl-get-text-highlight-face)))
      (ignore-errors
        (if (equal matched-face face)
            (progn
              (forward-char step)
              ;; Recursive call.
              (hl-seek-matched-face matched-face step))
          (backward-char step))))))

(defun hl-font-lock-keyword-p (regexp)
  (let ((keyword (assoc regexp (if (eq t (car font-lock-keywords))
                                   (cadr font-lock-keywords)
                                 font-lock-keywords))))
    (if (eq 'prepend (nth 3 keyword))
        keyword nil)))

(defun hl-buffer-list (&optional only-file)
  (delq nil (mapcar (lambda (buffer)
                      (if only-file
                          (and (buffer-live-p buffer)
                               (buffer-file-name buffer)
                               buffer)
                        (and (buffer-live-p buffer)
                             buffer)))
                    (buffer-list))))

(defmacro hl-highlight-internal (regexp database index)
  "Use `font-lock-add-keywords' to add keywords and add overlays for specific 
FACESPEC just at current line. See `hl-add-highlight-overlays'."
  (declare (indent 0) (debug t))
  `(let* ((max (max (length hl-highlight-foreground-colors)
                    (length hl-highlight-background-colors)))
          (next-index (1+ ,index))
          (new-index (if (>= next-index max) 0 next-index))
          (fg (nth ,index hl-highlight-foreground-colors))
          (bg (nth ,index hl-highlight-background-colors))
          facespec)
     ;; Prepare face for highlight.
     (and fg (push `(foreground-color . ,fg) facespec))
     (and bg (push `(background-color . ,bg) facespec))
     ;; Save highlight into database.
     (push ,regexp ,database)
     ;; Update index of colors.
     (setq ,index new-index)
     ;; Fontify buffer(s).
     (mapc (lambda (buffer)
             (with-current-buffer buffer
               (font-lock-add-keywords nil
                                       `((,regexp 0 ',facespec prepend))
                                       'append)
               (font-lock-fontify-buffer)))
           ;; global or local?
           (if (eq ,database hl-highlights)
               (hl-buffer-list)
             `(,(current-buffer))))))

(defmacro hl-unhighlight-internal (regexp database index)
  "Use `font-lock-remove-keywords' to remove keywords."
  (declare (indent 0) (debug t))
  `(let ((keyword (hl-font-lock-keyword-p ,regexp)))
     ;; Remove highlight from database.
     (setq ,database (delete ,regexp ,database))
     ;; Update index of colors.
     (unless ,database
       (setq ,index 0))
     ;; Fontify buffer(s).
     (mapc (lambda (buffer)
             (with-current-buffer buffer
               (while (setq keyword (hl-font-lock-keyword-p ,regexp))
                 (font-lock-remove-keywords nil (list keyword)))
               (font-lock-fontify-buffer)))
           ;; global or local?
           (if (eq ,database hl-highlights)
               (hl-buffer-list)
             `(,(current-buffer))))))

(defun hl-sync-global-highlights ()
  "Synchronize global highlights at `find-file-hook'."
  (let ((index 0)
        highlights)
    (dolist (regexp (reverse hl-highlights))
      (hl-highlight-internal regexp highlights index))))

(defun hl-highlight-pre-command ()
  "Remove temporary highlights and cancel `hl-timer'."
  (unless (and (featurep 'edebug) edebug-active)
    ;; Cancel idle timer.
    (when hl-timer
      (cancel-timer hl-timer)
      (setq hl-timer nil))))

(defun hl-highlight-post-command ()
  "Use idle timer, `hl-timer' to update overlays for highlights if it's not in 
the `edebug-mode'."
  (unless (and (featurep 'edebug) edebug-active)
    (when (hl-begin?)
      (setq hl-timer (run-with-idle-timer 0 nil 'hl-add-highlight-overlays)))))

(defun hl-begin? ()
  (not (or (active-minibuffer-window))))

(defun hl-add-highlight-overlays ()
  "Add overlays only for things at current line when `hl-line-mode' or 
`global-hl-line-mode' is enabled.
Note: It is called by highlight engine in `post-command-hook'. You shound't 
call this function directly!"
  (when (or (and (or hl-line-mode global-hl-line-mode)
                 (or hl-highlights
                     hl-highlights-local
                     hl-highlight-special-faces
                     hl-overlays)))
    ;; Remove overlays.
    (mapc 'delete-overlay hl-overlays)
    (setq hl-overlays nil)
    ;; Create overlays.
    (let ((hl-region (and (region-active-p)
                          (cons (region-beginning) (region-end))))
          (end (line-end-position))
          bound)
      (save-excursion
        (beginning-of-line)
        (while (and (<= (point) end)
                    (not (eobp)))
          (if (setq bound (hl-bounds-of-highlight))
              ;; TODO: Add overlay's priority.
              (let ((overlay (make-overlay (point) (cdr bound)))
                    (face (hl-get-text-highlight-face)))
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
                (push overlay hl-overlays)
                (goto-char (cdr bound)))
            (forward-char)))))))

;;;###autoload
(defun hl-highlight-thingatpt-global ()
  "Toggle global highlight."
  (interactive)
  (unless hl-highlight-mode
    (hl-highlight-mode 1))
  (let* ((thing (hl-thingatpt))
         (regexp (car thing)))
    (and thing
         (if (member regexp hl-highlights)
             (hl-unhighlight-internal regexp
                                      hl-highlights hl-colors-index)
           (hl-highlight-internal regexp
                                  hl-highlights hl-colors-index)))))

;;;###autoload
(defun hl-unhighlight-all-global ()
  "Remove all global highlights."
  (interactive)
  (dolist (regexp hl-highlights)
    (hl-unhighlight-internal regexp
                             hl-highlights hl-colors-index))
  (setq hl-colors-index 0))

;;;###autoload
(defun hl-highlight-thingatpt-local ()
  "Toggle local highlights in the current buffer."
  (interactive)
  (unless hl-highlight-mode
    (hl-highlight-mode 1))
  (let* ((thing (hl-thingatpt))
         (regexp (car thing)))
    (and thing
         (if (member regexp hl-highlights-local)
             (hl-unhighlight-internal regexp
                                      hl-highlights-local hl-colors-index-local)
           (hl-highlight-internal regexp
                                  hl-highlights-local hl-colors-index-local)))))

;;;###autoload
(defun hl-unhighlight-all-local ()
  "Remove all local highlights in buffer."
  (interactive)
  (dolist (regexp hl-highlights-local)
    (hl-unhighlight-internal regexp
                             hl-highlights-local hl-colors-index-local))
  (setq hl-colors-index-local 0))

;;;###autoload
(defun hl-save-highlights ()
  "Save highlights in `hl-highlight-save-file' file.

  (:global HL-HIGHLIGHTS                 ;; `hl-highlights'
   :local (FILE . HL-HIGHLIGHTS-LOCAL))  ;; `hl-highlights-local'

You can call `hl-restore-highlights' to revert highlights of last session."
  (interactive)
  (let (save)
    ;; Save global highlights.
    (setq save (plist-put save :global (reverse hl-highlights)))
    ;; Save local highlights.
    (let (local)
      (dolist (buffer (hl-buffer-list t))
        (with-current-buffer buffer
          (and hl-highlights-local
               (push (cons (buffer-file-name)
                           (reverse hl-highlights-local))
                     local))))
      (setq save (plist-put save :local local)))
    ;; Export.
    (hl-export hl-highlight-save-file save)))

;;;###autoload
(defun hl-restore-highlights ()
  "Load highligts from `hl-highlight-save-file' file. Before calling this, you 
could call `hl-save-highlights' function."
  (interactive)
  (unless hl-highlight-mode
    (hl-highlight-mode 1))
  ;; Import.
  (let ((save (hl-import hl-highlight-save-file)))
    ;; Restore global highlights.
    (hl-unhighlight-all-global)
    (let ((highlights (plist-get save :global)))
      (dolist (regexp highlights)
        (hl-highlight-internal regexp
                               hl-highlights hl-colors-index)))
    ;; Restore local highlights.
    (let ((local (plist-get save :local))
          highlights)
      (dolist (buffer (hl-buffer-list t))
        (with-current-buffer buffer
          (when (setq highlights (assoc (buffer-file-name) local))
            (hl-unhighlight-all-local)
            (dolist (regexp (cdr highlights))
              (hl-highlight-internal regexp
                                     hl-highlights-local
                                     hl-colors-index-local))))))))

;;;###autoload
(define-minor-mode hl-highlight-mode
  "Enable highligt engine to do:
- Show highlight over current line highlight (`hl-line-mode' or 
  `global-hl-line-mode').
- Synchronize global highlights.
- Save highlights before killing Emacs and restore them next time."
  :lighter " hl-highlight"
  :global t
  (if hl-highlight-mode
      (progn
        (add-hook 'pre-command-hook 'hl-highlight-pre-command t)
        (add-hook 'post-command-hook 'hl-highlight-post-command t)
        (add-hook 'find-file-hook 'hl-sync-global-highlights t)
        (add-hook 'kill-emacs-hook 'hl-save-highlights t)
        ;; Load saved highlights and add `kill-emacs-hook' when module is
        ;; loaded 1st time.
        (and load-file-name
             hl-auto-save-restore-highlights
             (hl-restore-highlights))
        ;; 1st time to add highlights overlays.
        (hl-add-highlight-overlays))
    ;; Remove overlays.
    (dolist (buffer (buffer-list))
      (mapc 'delete-overlay hl-overlays)
      (setq hl-overlays nil))
    ;; Cancel timer and hook.
    (when hl-timer
      (cancel-timer hl-timer)
      (setq hl-timer nil))
    (remove-hook 'pre-command-hook 'hl-highlight-pre-command)
    (remove-hook 'post-command-hook 'hl-highlight-post-command)
    (remove-hook 'find-file-hook 'hl-sync-global-highlights)
    (remove-hook 'kill-emacs-hook 'hl-save-highlights)))

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
      (deactivate-mark t)
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
      (activate-mark)
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
  "Foreground colors for outward parentheses highlights."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-paren)

(defcustom hl-outward-paren-bg-colors '("cyan"
                                        "gold")
  "Background colors for outward parentheses highlights."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-paren)

(defcustom hl-inward-paren-fg-color "snow"
  "Foreground colors for inward the parentheses highlights."
  :type 'color
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-paren)

(defcustom hl-inward-paren-bg-color "magenta1"
  "Background colors for inward parentheses highlights."
  :type 'color
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-paren)

(defface hl-paren-face nil
  "Template face used for parentheses highlight."
  :group 'hl-paren)

(defvar hl-paren-timer nil)

(defvar hl-outward-parens nil)
(make-variable-buffer-local 'hl-outward-parens)

(defvar hl-inward-parens nil)
(make-variable-buffer-local 'hl-inward-parens)

(defun hl-paren-idle-begin ()
  (when (hl-paren-is-begin)
    (setq hl-paren-timer (run-with-idle-timer 0.1 nil 'hl-create-parens))))

(defun hl-paren-is-begin ()
  (not (or (active-minibuffer-window))))

(defun hl-create-parens ()
  "Highlight enclosing parentheses."
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
  "Minor mode to highlight the enclosing parentheses and more."
  :lighter " hl-paren"
  (if hl-paren-mode
      (progn
        (add-hook 'pre-command-hook 'hl-remove-parens nil t)
        (add-hook 'post-command-hook 'hl-paren-idle-begin nil t))
    (remove-hook 'pre-command-hook 'hl-remove-parens t)
    (remove-hook 'post-command-hook 'hl-paren-idle-begin t)))

(provide 'hl-anything)
;;; hl-anything.el ends here
