;;; vs-edit-mode.el --- Minor mode accomplish editing experience in Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh
;; Created date 2022-03-11 22:10:58

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/vs-edit-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (mwim "0.4") (ts-fold "0.1.0") (noflet "0.0.15"))
;; Keywords: convenience editing vs

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Minor mode accomplish editing experience in Visual Studio.
;;

;;; Code:

(eval-when-compile
  (require 'mwim)
  (require 'ts-fold)
  (require 'noflet))

(require 'sgml-mode)

(defgroup vs-edit nil
  "Minor mode accomplish editing experience in Visual Studio."
  :prefix "vs-edit-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/vs-edit-mode"))

(defcustom vs-edit-active-modes
  '( actionscript-mode
     c-mode c++-mode csharp-mode objc-mode
     css-mode
     go-mode
     groovy-mode
     haxe-mode
     java-mode
     javascript-mode js-mode js2-mode js3-mode
     jenkinsfile-mode
     json-mode
     perl-mode
     rjsx-mode
     rust-mode
     shader-mode
     shell-mode
     svelte-mode
     typescript-mode
     tsx-mode
     web-mode html-mode sgml-mode
     xml-mode nxml-mode)
  "List of major mode to active minor mode, `vs-edit-mode'."
  :type 'list
  :group 'vs-edit)

;;
;; (@* "Externals" )
;;

(defvar tree-sitter-tree)
(declare-function tsc-node-start-position "ext:tsc.el")
(declare-function tsc-node-end-position "ext:tsc.el")
(declare-function ts-fold--foldable-node-at-pos "ext:ts-fold.el")

(declare-function lsp-format-buffer "ext:lsp-mode.el")

;;
;; (@* "Entry" )
;;

(defvar vs-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "{") #'vs-edit-opening-curly-bracket-key)
    (define-key map (kbd ";") #'vs-edit-semicolon-key)
    (define-key map (kbd "#") #'vs-edit-sharp-key)
    (define-key map (kbd "<up>") #'vs-edit-previous-line)
    (define-key map (kbd "<down>") #'vs-edit-next-line)
    map)
  "Keymap used in function `vs-edit-mode'.")

(defun vs-edit-mode--enable ()
  "Enable function `vs-edit-mode'."
  (advice-add 'newline :around #'vs-edit-newline)
  (advice-add 'newline-and-indent :around #'vs-edit-newline-and-indent))

;;;###autoload
(define-minor-mode vs-edit-mode
  "Minor mode `vs-edit-mode'."
  :lighter " VS-Edit"
  :group vs-edit
  (when vs-edit-mode
    (vs-edit-mode--enable)
    (unless (memq major-mode vs-edit-active-modes)
      (vs-edit-mode -1))))

(defun vs-edit-turn-on-mode ()
  "Turn on the `vs-edit-mode'."
  (vs-edit-mode 1))

;;;###autoload
(define-globalized-minor-mode global-vs-edit-mode
  vs-edit-mode vs-edit-turn-on-mode
  :group 'vs-edit
  :require 'vs-edit)

;;
;; (@* "Util" )
;;

(defmacro vs-edit--point-at-pos (&rest body)
  "Execute BODY when return point."
  (declare (indent 0) (debug t))
  `(save-excursion ,@body (point)))

(defun vs-edit--comment-p ()
  "Return non-nil if it's inside comment or string."
  (nth 4 (syntax-ppss)))

(defun vs-edit--comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (vs-edit--comment-p) (nth 8 (syntax-ppss))))

(defun vs-edit--delete-region ()
  "Delete region by default value."
  (when (use-region-p) (delete-region (region-beginning) (region-end))))

(defun vs-edit--current-line-totally-empty-p ()
  "Current line empty with no spaces/tabs in there.  (absolute)."
  (and (bolp) (eolp)))

(defun vs-edit--current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun vs-edit--current-char-string ()
  "Get the current character as the `string'."
  (if (char-before) (string (char-before)) ""))

(defun vs-edit--current-char-equal-p (c)
  "Check the current character equal to C, C can be a list of character."
  (cond ((and (stringp c) (stringp (vs-edit--current-char-string)))
         (string= (vs-edit--current-char-string) c))
        ((listp c) (member (vs-edit--current-char-string) c))))

(defun vs-edit--current-whitespace-or-tab-p ()
  "Check if current character a whitespace or a tab character?"
  (vs-edit--current-char-equal-p '(" " "\t")))

(defun vs-edit--infront-first-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing infront of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-backward "[^ \t]" (line-beginning-position) t))))

(defun vs-edit--behind-last-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing behind of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-forward "[^ \t]" (line-end-position) t))))

(defun vs-edit--before-char-string ()
  "Get the current character as the `string'."
  (if (char-before) (string (char-before)) ""))

(defun vs-edit--first-backward-char-in-line-p (ch)
  "Return t if the CH is the first character on the left in line."
  (save-excursion
    (when (re-search-backward "[^[:space:]]" (line-beginning-position) t)
      (forward-char 1)
      (string= (vs-edit--before-char-string) ch))))

;;
;; (@* "Core" )
;;

(defun vs-edit--tag-on-line-p ()
  "Return non-nil when tag is on the end of this line."
  (let ((end (line-end-position)))
    (save-excursion
      (sgml-skip-tag-forward 1)
      (and (eolp) (<= (point) end)))))

(defun vs-edit-newline (func &rest args)
  "Advice for function `newline' (FUNC and ARGS)."
  (cond (vs-edit-mode
         (let ((vs-edit-mode)
               (behind-brackets (vs-edit--first-backward-char-in-line-p "{")))
           ;; XXX: Make sure indent on the empty line.
           (when (vs-edit--current-line-totally-empty-p) (indent-for-tab-command))
           ;; XXX: Maintain same indentation for the previous line.
           (let ((ln-cur (buffer-substring (line-beginning-position) (point))))
             (apply func args)
             (save-excursion
               (forward-line -1)
               (when (vs-edit--current-line-totally-empty-p) (insert ln-cur))))
           ;; XXX: Make sure brackets on newline!
           (if (or (and behind-brackets
                        (string= "}" (string-trim (thing-at-point 'line))))
                   (and (derived-mode-p 'sgml-mode)
                        (vs-edit--tag-on-line-p)))
               (save-excursion (newline-and-indent))
             ;; Else, make sure next line is indented
             (save-excursion (forward-line 1) (indent-for-tab-command)))))
        (t (apply func args))))

(defun vs-edit-newline-and-indent (func &rest args)
  "Advice for function `newline-and-indent' (FUNC and ARGS)."
  (cond (vs-edit-mode
         ;; XXX: Don't delete previous line' trailing whitespaces!
         (noflet ((delete-horizontal-space (&rest _)))  ; see function `newline-and-indent' implementation
           (apply func args)))
        (t (apply func args))))

(defun vs-edit-opening-curly-bracket-key ()
  "For programming langauge that need `{`."
  (interactive)
  (cond ((use-region-p)
         (self-insert-command 1 ?\{))
        ((vs-edit--comment-or-string-p)
         (insert "{"))
        (t
         (let (pretty-it space-infront)
           (when (ignore-errors (looking-back "{ " 2))
             (delete-char -1))

           (unless (vs-edit--current-char-equal-p "{")
             (setq pretty-it t)
             (when (and (not (vs-edit--current-whitespace-or-tab-p))
                        (not (vs-edit--current-char-equal-p '("(" "[" ":" "=" ">"))))
               (setq space-infront t)))

           (when space-infront (insert " "))

           (insert "{ }")
           (backward-char 1)
           (indent-for-tab-command)

           (when pretty-it
             (save-excursion
               (forward-char 2)
               (when (and (not (eobp))
                          (not (bolp))
                          (vs-edit--current-char-equal-p "}"))
                 (backward-char 1)
                 (insert " "))))))))

(defun vs-edit-semicolon-key ()
  "For programming language that use semicolon as the end operator sign."
  (interactive)
  (vs-edit--delete-region)
  (insert ";")
  (unless (vs-edit--comment-or-string-p)
    (save-excursion
      (forward-char 1)
      (when (and (not (bolp))
                 (vs-edit--current-char-equal-p "}"))
        (backward-char 1)
        (insert " ")))))

(defun vs-edit-sharp-key ()
  "For programming language that use # as the preprocessor."
  (interactive)
  (vs-edit--delete-region)
  (insert "#")
  (backward-char 1)
  (when (vs-edit--infront-first-char-at-line-p)
    (kill-region (line-beginning-position) (point)))
  (forward-char 1))

;;
;; (@* "Navigation" )
;;

(defun vs-edit--after-move-line ()
  "Do stuff after smart move line."
  (cond ((vs-edit--current-line-empty-p) (end-of-line))
        ((and (vs-edit--infront-first-char-at-line-p)
              (re-search-forward "[^[:space:]\t]" (line-end-position) t))
         (forward-char -1))))

;;;###autoload
(defun vs-edit-previous-line ()
  "Smart way to navigate to previous line."
  (interactive)
  (call-interactively #'previous-line)
  (vs-edit--after-move-line))

;;;###autoload
(defun vs-edit-next-line ()
  "Smart way to navigate to next line."
  (interactive)
  (call-interactively #'next-line)
  (vs-edit--after-move-line))

;;;###autoload
(defun vs-edit-backward-word (&optional _)
  "Smart backward a word."
  (interactive "^P")
  (let ((start-pt (point)) (start-ln (line-number-at-pos))
        (beg-ln (bolp))
        (infront-first-char (vs-edit--infront-first-char-at-line-p)))
    (backward-word 1)
    (cond ((and infront-first-char (not beg-ln))
           (goto-char start-pt)
           (beginning-of-line))
          ((and (not (= start-ln (line-number-at-pos))) (not beg-ln))
           (goto-char start-pt)
           (mwim-beginning-of-code-or-line))
          ((>= (abs (- start-ln (line-number-at-pos))) 2)
           (goto-char start-pt)
           (forward-line -1)
           (end-of-line)))))

;;;###autoload
(defun vs-edit-forward-word (&optional _)
  "Smart forward a word."
  (interactive "^P")
  (let ((start-pt (point))
        (start-ln (line-number-at-pos))
        (behind-last-char (vs-edit--behind-last-char-at-line-p)))
    (forward-word 1)
    (cond ((and (not (= start-ln (line-number-at-pos)))
                (not behind-last-char))
           (goto-char start-pt)
           (end-of-line))
          ((>= (abs (- start-ln (line-number-at-pos))) 2)
           (goto-char start-pt)
           (forward-line 1)
           (mwim-beginning-of-code-or-line)))))

;;;###autoload
(defun vs-edit-backward-delete-word ()
  "Backward deleteing ARG words in the smart way."
  (interactive)
  (if (use-region-p) (vs-edit--delete-region)
    (let ((start-pt -1) (end-pt (point)) (start-ln-end-pt -1))
      (save-excursion
        (vs-edit-backward-word)
        (setq start-pt (point)
              start-ln-end-pt (line-end-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq start-pt start-ln-end-pt))
      (delete-region start-pt end-pt))))

;;;###autoload
(defun vs-edit-forward-delete-word ()
  "Forward deleteing ARG words in the smart way."
  (interactive)
  (if (use-region-p) (vs-edit--delete-region)
    (let ((start-pt (point)) (end-pt -1) (end-ln-start-pt -1))
      (save-excursion
        (vs-edit-forward-word)
        (setq end-pt (point)
              end-ln-start-pt (line-beginning-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq end-pt end-ln-start-pt))
      (delete-region start-pt end-pt))))

;;
;; (@* "Format" )
;;

;;;###autoload
(defun vs-edit-format-document ()
  "Format current document."
  (interactive)
  (message nil)  ; avoid scroll mess up
  (or (and (bound-and-true-p lsp-managed-mode)
           (ignore-errors (lsp-format-buffer)))
      (indent-region (point-min) (point-max))))

;;;###autoload
(defun vs-edit-format-region-or-document ()
  "Format the document if there are no region apply."
  (interactive)
  (cond ((use-region-p)
         (message nil)  ; avoid scroll mess up
         (indent-region (region-beginning) (region-end)))
        (t
         (vs-edit-format-document))))

;;
;; (@* "Folding" )
;;

(defun vs-edit--range-at-pos ()
  "Return the range at position."
  (require 'ts-fold)
  (when-let ((tree-sitter-tree)
             (node (ts-fold--foldable-node-at-pos)))
    (cons (tsc-node-start-position node) (tsc-node-end-position node))))

(defun vs-edit--range-diff ()
  "Return the range diff at position."
  (when-let ((range (vs-edit--range-at-pos)))
    (- (cdr range) (car range))))

(defun vs-edit--end-of-line-before-comment ()
  "Move to end of line but avoid the comment."
  (end-of-line)
  (ignore-errors (comment-beginning)))

(defun vs-edit--to-smallest-range ()
  "Move to smallest range."
  (let ((bol-diff (save-excursion (beginning-of-line) (vs-edit--range-diff)))
        (eol-diff (save-excursion  (vs-edit--end-of-line-before-comment)
                                   (vs-edit--range-diff))))
    (cond ((and bol-diff (null eol-diff))
           (beginning-of-line))
          ((and eol-diff (null bol-diff))
           (vs-edit--end-of-line-before-comment))
          ((and bol-diff eol-diff)
           (if (< bol-diff eol-diff)
               (beginning-of-line)
             (vs-edit--end-of-line-before-comment)))
          (t  ; Do nothing if no fold range found!
           ))))

(defun vs-edit--close-node ()  ; internal
  "Close node at the end of line, inspired from Visual Studio."
  (save-excursion
    (vs-edit--to-smallest-range)
    (when (vs-edit--comment-p) (back-to-indentation))
    (ts-fold-close)))

(defun vs-edit--open-node ()  ; internal
  "Open node at the end of line, inspired from Visual Studio."
  (save-excursion
    (vs-edit--to-smallest-range)
    (when (vs-edit--comment-p) (back-to-indentation))
    (let ((before-pt (vs-edit--point-at-pos (beginning-of-visual-line)))
          after-pt
          result)
      (setq result (ts-fold-open))
      (setq after-pt (vs-edit--point-at-pos (beginning-of-visual-line)))
      (unless (= after-pt before-pt)
        (goto-char before-pt)
        (end-of-line))
      result)))

;;;###autoload
(defun vs-edit-close-node ()
  "Close the current scope of the node."
  (interactive)
  (when-let* ((ov (or (vs-edit--close-node) (ts-fold-close)))
              (beg (overlay-start ov))
              ((< beg (point))))
    (goto-char beg)))

;;;###autoload
(defun vs-edit-open-node ()
  "Open the current scope of the node."
  (interactive)
  (or (vs-edit--open-node) (ts-fold-open)))

(provide 'vs-edit-mode)
;;; vs-edit-mode.el ends here
