;;; hl-anything.el --- Highlight symbols, selections, enclosing parens and more.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 20141107.1845
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
;; Highlight things in a text file makes you search things easily. It is
;; fundamental and very helpful to everyone, enjoy!
;;
;; Check website for details:
;; https://github.com/boyw165/hl-anything
;; 
;; * Highlight symbols with different colors.
;;   Note: The highlights are still visible even under current line highlight
;;   (`hl-line-mode' or `global-hl-line-mode' is enabled).
;; * Highlight selections with different colors.
;; * Highlight things in a highlighted thing.
;; * Highlight enclosing inward and outward parentheses.
;; * Select highlighted things smartly and search forwardly or backwardly.
;;
;; Usage:
;; ------
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
;; Enable enclosing parenethese highlighting:
;;   M-x `hl-paren-mode'
;;
;; Highlight things temporarily which means any action will delete the highlights.
;;  (hl-highlight-keywords-temporarily '(("hello" hl-symbol-face)))
;;
;; TODO:
;; -----
;; * Implement `hl-highlight-thingatpt-global' to highlight things globally.
;; * Save highlights before Emacs closed in order to restore them after Emacs
;;   opened?
;; * Highlight Enclosing syntax in Emacs REGEX.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-12-25
;; * Rename `hl-fg-colors' to `hl-highlight-foreground-colors'.
;;          `hl-bg-colors' to `hl-highlight-background-colors'.
;;   Note: Users should follow this change!
;; * Support `hl-highlight-thingatpt-global' and `hl-unhighlight-all-global'.
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
;; * Initial release, fork from http://nschum.de/src/emacs/highlight-parentheses.
;;
;;; Code:

;; GNU Library.
(require 'thingatpt)

(defgroup hl-anything nil
  "Highlight anything."
  :tag "hl-anything"
  :group 'faces
  :group 'font-lock
  :group 'matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface hl-file-face
  '((t (:foreground "blue" :underline t :weight bold)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-number-face
  '((t (:foreground "maroon1")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-generic-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-local-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-global-variable-face
  '((t (:foreground "black")))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-function-parameter-face
  '((t (:underline t)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-symbol-face
  '((t (:background "gold" :foreground "black" :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-title-1-face
  '((t (:background "LightCyan3" :foreground "gray40" :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-title-2-face
  '((t (:background "LightCyan2" :foreground "gray40" :weight bold :height 1.3)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

(defface hl-title-3-face
  '((t (:background "LightCyan1" :foreground "gray40" :weight bold :height 1.1)))
  "Default face for highlighting keyword in definition window."
  :group 'hl-anything)

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
  "The foreground colors for `hl-highlight-thingatpt'."
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
  "The background colors for `hl-highlight-thingatpt'."
  :type '(repeat color)
  :tag "Highlight Background Colors"
  :group 'hl-anything)

(defcustom hl-before-find-thing-hook nil
  "Hook for doing something before `hl-find-thing' do the searching.
This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature."
  :type '(repeat function)
  :group 'hl-anything)

(defcustom hl-after-find-thing-hook nil
  "Hook for doing something after `hl-find-thing' do the searching.
This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature."
  :type '(repeat function)
  :group 'hl-anything)

(defcustom hl-highlight-special-faces '(hl-symbol-face
                                        hl-title-1-face
                                        hl-title-2-face
                                        hl-title-3-face)
  "For the faces that will be treat as highlights, which means overlays 
will also be created for these faces under current line."
  :type '(repeat face)
  :group 'hl-anything)

(defcustom hl-highlight-save-file "~/.emacs.d/.hl-save"
  "A file storing information of highlights in the last session."
  :type 'string
  :group 'hl-anything)

(defvar hl-timer nil)

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

(defvar hl-temp-keywords nil
  "A local keywords list. See `font-lock-keywords' for its format.")
(make-variable-buffer-local 'hl-temp-keywords)

(defvar hl-is-highlight-special-faces nil
  "Force to create `hl-overlays' overlays.")
(make-variable-buffer-local 'hl-is-highlight-special-faces)

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

(defun hl-buffer-list (&optional ignore)
  (delq nil (mapcar (lambda (buffer)
                      (unless (and ignore
                                   (string-match "\*.*\*$" (buffer-name buffer)))
                        (and (buffer-live-p buffer)
                             buffer)))
                    (buffer-list))))

(defun hl-sync-things-global ()
  ;; TODO:
  )

(defun hl-highlight-internal (regexp &optional global)
  (let* ((max (max (length hl-highlight-foreground-colors)
                   (length hl-highlight-background-colors)))
         (index (if global hl-colors-index hl-colors-index-local))
         (next-index (1+ index))
         (new-index (if (>= next-index max) 0 next-index))
         (fg (nth index hl-highlight-foreground-colors))
         (bg (nth index hl-highlight-background-colors))
         facespec)
    ;; Prepare face for highlight.
    (and fg (push `(foreground-color . ,fg) facespec))
    (and bg (push `(background-color . ,bg) facespec))
    ;; Save highlight into database.
    (push regexp (if global
                     hl-highlights
                   hl-highlights-local))
    ;; Update index of colors.
    (if global
        (setq hl-colors-index new-index)
      (setq hl-colors-index-local new-index))
    ;; Fontify buffer(s).
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (font-lock-add-keywords nil `((,regexp 0 ',facespec prepend)) 'append)
              (font-lock-fontify-buffer)))
          (if global
              (hl-buffer-list t)
            (list (current-buffer))))))

(defun hl-unhighlight-internal (regexp &optional global)
  (let ((highlights (delete regexp (if global
                                       hl-highlights
                                     hl-highlights-local)))
        (keyword (hl-font-lock-keyword-p regexp)))
    ;; Remove highlight from database.
    (if global
        (setq hl-highlights highlights)
      (setq hl-highlights-local highlights))
    ;; Update index of colors.
    (unless highlights
      (if global
          (setq hl-colors-index 0)
        (setq hl-colors-index-local 0)))
    ;; Fontify buffer(s).
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (while (setq keyword (hl-font-lock-keyword-p regexp))
                (font-lock-remove-keywords nil `(,keyword)))
              (font-lock-fontify-buffer)))
          (if global
              (hl-buffer-list t)
            (list (current-buffer))))))

(defun hl-font-lock-keyword-p (regexp)
  (let ((keyword (assoc regexp (if (eq t (car font-lock-keywords))
                                   (cadr font-lock-keywords)
                                 font-lock-keywords))))
    (if (eq 'prepend (nth 3 keyword))
        keyword nil)))

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
  "Add overlays only for things at current line when `hl-line-mode' or 
`global-hl-line-mode' is enabled.
Note: It is called by highlight engine in `post-command-hook'. You shound't 
call this function directly!"
  (when (or (and hl-highlight-mode
                 (require 'hl-line) (or hl-line-mode global-hl-line-mode)
                 (or hl-highlights
                     hl-highlights-local
                     hl-overlays
                     hl-temp-keywords))
            hl-is-highlight-special-faces)
    ;; Remove overlays.
    (mapc 'delete-overlay hl-overlays)
    (setq hl-overlays nil)
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
                (push overlay hl-overlays)
                (goto-char (cdr bound)))
            (forward-char)))))))

;;;###autoload
(defun hl-highlight-thingatpt-global ()
  "Toggle highlighting globally."
  (interactive)
  (unless hl-highlight-mode
    (hl-highlight-mode 1))
  (let* ((thing (hl-thingatpt))
         (regexp (car thing)))
    (and thing
         (if (member regexp hl-highlights)
             (hl-unhighlight-internal regexp t)
           (hl-highlight-internal regexp t))))
  (unless (default-value 'hl-highlights)
    (remove-hook 'find-file-hook 'hl-sync-things-global t)))

;;;###autoload
(defun hl-highlight-thingatpt-local ()
  "Toggle highlighting locally in the current buffer."
  (interactive)
  (unless hl-highlight-mode
    (hl-highlight-mode 1))
  (let* ((thing (hl-thingatpt))
         (regexp (car thing)))
    (and thing
         (if (member regexp hl-highlights-local)
             (hl-unhighlight-internal regexp)
           (hl-highlight-internal regexp)))))

;;;###autoload
(defun hl-highlight-keywords-temporarily (keywords)
  "Highlight KEYWORDS locally and temporarily in the current buffer. Any action
 will remove the temporary highlights. See `font-lock-keywords'."
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
  (dolist (regexp hl-highlights-local)
    (hl-unhighlight-internal regexp))
  (setq hl-colors-index-local 0))

;;;###autoload
(defun hl-unhighlight-all-global ()
  "Remove all the highlights in buffer."
  (interactive)
  (dolist (regexp hl-highlights)
    (hl-unhighlight-internal regexp t))
  (setq hl-colors-index 0))

;;;###autoload
(define-minor-mode hl-highlight-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " hl-highlight"
  :global t
  (if hl-highlight-mode
      (progn
        (add-hook 'pre-command-hook 'hl-highlight-pre-command t)
        (add-hook 'post-command-hook 'hl-highlight-post-command t)
        (hl-add-highlight-overlays))
    (remove-hook 'pre-command-hook 'hl-highlight-pre-command)
    (remove-hook 'post-command-hook 'hl-highlight-post-command)))

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
  "List of colors for the highlighted parentheses. The list starts with the 
the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything)

(defcustom hl-outward-paren-bg-colors '("cyan"
                                        "gold")
  "List of colors for the background highlighted parentheses. The list starts 
with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything)

(defcustom hl-inward-paren-fg-color "snow"
  "List of colors for the background highlighted parentheses. The list starts 
with the the inside parentheses and moves outwards."
  :type 'color
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything)

(defcustom hl-inward-paren-bg-color "magenta1"
  "List of colors for the background highlighted parentheses. The list starts 
with the the inside parentheses and moves outwards."
  :type 'color
  :initialize 'custom-initialize-default
  :set 'hl-paren-custom-set
  :group 'hl-anything)

(defface hl-paren-face nil
  "Face used for highlighting parentheses."
  :group 'hl-anything)

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
