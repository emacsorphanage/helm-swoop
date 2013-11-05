;;; helm-swoop.el --- Efficiently hopping squeezed lines powered by helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013 by Shingo Fukuyama

;; Version: 1.2
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/helm-swoop
;; Created: Oct 24 2013
;; Keywords: helm swoop inner buffer search
;; Package-Requires: ((helm "1.0") (emacs "24"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:

;; List the all lines to another buffer, which is able to squeeze
;; by any words you input. At the same time, the original buffer's
;; cursor is jumping line to line according to moving up and down
;; the list.

;; Example config
;; ----------------------------------------------------------------
;; ;; helm from https://github.com/emacs-helm/helm
;; (require 'helm-config)
;; (helm-mode 1)

;; ;; Locate the helm-swwop folder to your path
;; ;; This line is unnecessary if you get this program from MELPA
;; (add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")

;; (require 'helm-swoop)

;; ;; Change the keybinds to whatever you like :)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

;; ;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; ----------------------------------------------------------------

;; Helm Swoop Edit
;; While doing helm-swoop, press keybind [C-c C-e] to move to edit buffer.
;; Edit the list and apply by [C-x C-s]. If you'd like to cancel, [C-c C-g]

;;; Code:

(eval-when-compile (require 'cl))

(require 'helm)
(require 'helm-swoop-edit)

(declare-function migemo-search-pattern-get "migemo")

(defgroup helm-swoop nil
  "Open helm-swoop."
  :prefix "helm-swoop-" :group 'helm)

(defface helm-swoop-target-line-face
  '((t (:background "#e3e300" :foreground "#222222")))
  "Face for helm-swoop target line"
  :group 'helm-swoop)

(defface helm-swoop-target-line-block-face
  '((t (:background "#cccc00" :foreground "#222222")))
  "Face for target line"
  :group 'helm-swoop)

(defface helm-swoop-target-word-face
  '((t (:background "#7700ff" :foreground "#ffffff")))
  "Face for target line"
  :group 'helm-swoop)

(defvar helm-swoop-split-window-function
  (lambda ($buf)
    (when (one-window-p)
      ;;(split-window-horizontally)
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer $buf))
  "Change the way to split window only when `helm-swoop' is calling")

(defvar helm-swoop-at-screen-top helm-display-source-at-screen-top
  "For enable scrolling margin")

(defvar helm-swoop-store-scroll-margin helm-completion-window-scroll-margin
  "To change scroll margin according to multiple line number and restore")

(defvar helm-swoop-first-position nil
  "For keep line position when `helm-swoop' is called")

;; Avoid compile error for apply buffer local variable
(defvar helm-swoop-cache)
(defvar helm-swoop-last-point)
(defvar helm-swoop-last-query) ;; Last search query for resume
(defvar helm-swoop-last-prefix-number) ;; For multiline highlight

(defvar helm-swoop-synchronizing-window nil
  "Window object where `helm-swoop' called from")
(defvar helm-swoop-target-buffer nil
  "Buffer object where `helm-swoop' called from")
(defvar helm-swoop-line-overlay nil
  "Overlay object to indicates other window's line")

(defun helm-swoop-back-to-last-point ()
  (interactive)
  "Go back to last position where `helm-swoop' was called"
  (if (and (boundp 'helm-swoop-last-point)
           helm-swoop-last-point)
    (let (($po (point)))
      (goto-char helm-swoop-last-point)
      (setq helm-swoop-last-point $po)))
  (message "There is no last point. Use this again after `helm-swoop' call"))

(defun helm-swoop-goto-line ($line)
  (goto-char (point-min))
  (unless (search-forward "\n" nil t (1- $line))
    (goto-char (point-max))))

(defun helm-swoop-delete-overlay (&optional $beg $end)
  (or $beg (setq $beg (point-min)))
  (or $end (setq $end (point-max)))
  (dolist ($o (overlays-in $beg $end))
    (if (overlay-get $o 'helm-swoop-target-word-face)
        (delete-overlay $o))))

(defun helm-swoop-get-string-at-line ()
  "Get string at the line."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun helm-swoop-target-line-overlay ()
  "Add color to the target line"
  (overlay-put
   (setq helm-swoop-line-overlay
         (make-overlay
          ;; Set beginning of highlight if multiline selected
          (progn
            (search-backward
             "\n" nil t (% (line-number-at-pos) helm-swoop-last-prefix-number))
            (goto-char (point-at-bol)))
          ;; For multiline highlight
          (save-excursion
            (goto-char (point-at-bol))
            (or (search-forward "\n" nil t helm-swoop-last-prefix-number)
                ;; For the end of the lines error
                (point-max)))))
   'face (if (< 1 helm-swoop-last-prefix-number)
             'helm-swoop-target-line-block-face
           'helm-swoop-target-line-face)))

;; helm action ------------------------------------------------

(defun helm-swoop--synchronizing-position ()
  (with-helm-window
    (let* (($key (helm-swoop-get-string-at-line))
           ($num (when (string-match "^[0-9]+" $key)
                   (string-to-number (match-string 0 $key)))))
      ;; Synchronizing line position
      (with-selected-window helm-swoop-synchronizing-window
        (if helm-swoop-first-position
            (progn
              (helm-swoop-goto-line $num)
              (with-current-buffer helm-swoop-target-buffer
                (delete-overlay helm-swoop-line-overlay)
                (helm-swoop-target-line-overlay))
              (recenter))
          ;; First action when helm-swoop calling
          (move-beginning-of-line 1)
          (helm-swoop-target-line-overlay)
          (recenter)
          (setq helm-swoop-first-position t))))))

(defun helm-swoop--pattern-match ()
  "Overlay target words"
  (with-helm-window
    (when (< 2 (length helm-pattern))
        (with-selected-window helm-swoop-synchronizing-window
          (helm-swoop-delete-overlay)
          (save-excursion
            (let (($pat (split-string helm-pattern " "))
                  $o)
              (dolist ($wd $pat)
                ;; Each word must be 3 or more of characters
                (when (< 2 (length $wd))
                  (goto-char (point-min))
                  ;; Optional require migemo.el & helm-migemo.el
                  (if (and (featurep 'migemo) (featurep 'helm-migemo))
                      (setq $wd (migemo-search-pattern-get $wd)))
                  ;; For caret begging match
                  (if (string-match "^\\^\\[0\\-9\\]\\+\\.\\(.+\\)" $wd)
                      (setq $wd (concat "^" (match-string 1 $wd))))

                  (while (re-search-forward $wd nil t)
                    (setq $o (make-overlay (match-beginning 0) (match-end 0)))
                    (overlay-put $o 'face 'helm-swoop-target-word-face)
                    (overlay-put $o 'helm-swoop-target-word-face t)
                    )))))))))

;; core ------------------------------------------------

(defun helm-swoop--get-content (&optional $linum)
  "Get the whole content in buffer and add line number at the head.
If $linum is number, lines are separated by $linum"
  (let (($bufstr
         (buffer-substring-no-properties (point-min) (point-max)))
        $return)
    (with-temp-buffer
      (insert $bufstr)
      (goto-char (point-min))
      (let (($i 1))
        (insert (format "%s " $i))
        (while (search-forward "\n" nil t)
          (incf $i)
          (insert (format "%s " $i))
          (when (and $linum (not (eq 0 (% $i $linum))))
              (re-search-backward "\n" nil t)
              ;; Special word for replace to "\n" later process
              (replace-match "@ff@")))
        (goto-char (point-min))
        ;; Delete empty lines
        (while (re-search-forward "^[0-9]+\\s-*$" nil t)
          (replace-match "")))
      (setq $return (buffer-substring-no-properties (point-min) (point-max))))
    $return))

(defvar helm-swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-e") 'helm-swoop-edit)
    (delq nil map))
  "Keymap for helm-M-colon")

(defun helm-c-source-swoop ()
  `((name . "Helm Swoop")
    (init . (lambda ()
              (unless helm-swoop-cache
                (with-current-buffer (helm-candidate-buffer 'local)
                  (insert ,(helm-swoop--get-content)))
                (setq helm-swoop-cache t))))
    (candidates-in-buffer)
    (keymap . ,helm-swoop-map)
    (header-line . "[C-c C-e] Edit mode")
    (action . (lambda ($line)
                (helm-swoop-goto-line
                 (when (string-match "^[0-9]+" $line)
                   (string-to-number (match-string 0 $line))))
                (when (re-search-forward
                       (mapconcat 'identity
                                  (split-string helm-pattern " ") "\\|")
                       nil t)
                  (goto-char (match-beginning 0)))
                (recenter)))
    (migemo) ;;? in exchange for those match ^ $ [0-9] .* for now
    ))

(defun helm-c-source-swoop-multiline ($linum)
  `((name . "Helm Swoop Multiline")
    (candidates . ,(let (($li (split-string
                                 (helm-swoop--get-content $linum) "\n")))
                       (mapcar (lambda ($x)
                                  (while (string-match "@ff@" $x)
                                    (setq $x (replace-match "\n" nil nil $x)))
                                  $x)
                               $li)))
    (keymap . ,helm-swoop-map)
    (action . (lambda ($line)
                (helm-swoop-goto-line
                 (when (string-match "^[0-9]+" $line)
                   (string-to-number (match-string 0 $line))))
                (when (re-search-forward
                       (mapconcat 'identity
                                  (split-string helm-pattern " ") "\\|")
                       nil t)
                  (goto-char (match-beginning 0)))
                (recenter)))
    (multiline)
    (migemo)))

(defvar helm-swoop-display-tmp helm-display-function
  "To restore helm window display function")

;; Delete cache when modified file is saved
(defun helm-swoop--clear-cache ()
  (if (boundp 'helm-swoop-cache) (setq helm-swoop-cache nil)))
(add-hook 'after-save-hook 'helm-swoop--clear-cache)

;;;###autoload
(defun helm-swoop (&optional $multiline $input)
  (interactive "p")
  "List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list."
  (setq helm-swoop-synchronizing-window (selected-window))
  (if (boundp 'helm-swoop-last-point)
      (setq helm-swoop-last-point (point))
    (set (make-local-variable 'helm-swoop-last-point) (point)))
  (unless (boundp 'helm-swoop-last-query)
    (set (make-local-variable 'helm-swoop-last-query) ""))
  (setq helm-swoop-last-point (point))
  (setq helm-swoop-target-buffer (current-buffer))
  (setq helm-swoop-line-overlay (make-overlay (point-at-bol) (point-at-eol)))
  ;; Enable scrolling margin
  (if (boundp 'helm-swoop-last-prefix-number)
      (setq helm-swoop-last-prefix-number
            (or $multiline 1)) ;; $multiline is for resume
    (set (make-local-variable 'helm-swoop-last-prefix-number)
         (or $multiline 1)))
  ;; Modify scroll tempolary
  (when helm-display-source-at-screen-top
    (setq helm-display-source-at-screen-top nil))
  (setq helm-completion-window-scroll-margin
          (+ 5 helm-swoop-last-prefix-number))
  ;; Cache
  (cond ((not (boundp 'helm-swoop-cache))
         (set (make-local-variable 'helm-swoop-cache) nil))
        ((not helm-swoop-cache)
         (setq helm-swoop-cache nil))
        ((buffer-modified-p)
         (setq helm-swoop-cache nil)))
  (unwind-protect
      (let (($line (helm-swoop-get-string-at-line)))
        ;; Modify window split function temporary
        (setq helm-display-function helm-swoop-split-window-function)
        ;; For synchronizing line position
        (add-hook 'helm-move-selection-after-hook
                  'helm-swoop--synchronizing-position)
        (add-hook 'helm-update-hook
                  'helm-swoop--pattern-match)
        ;; Execute helm
        (helm :sources
              (if (> helm-swoop-last-prefix-number 1)
                  (helm-c-source-swoop-multiline helm-swoop-last-prefix-number)
                (helm-c-source-swoop))
              :buffer "*Helm Swoop*"
              :input
              (cond ($input
                     (if (string-match
                          "\\(\\^\\[0\\-9\\]\\+\\.\\)\\(.*\\)" $input)
                         $input ;; NEED FIX #1 to appear as a "^"
                       $input))
                    (mark-active
                     (let (($st (buffer-substring-no-properties
                                 (region-beginning) (region-end))))
                       (if (string-match "\n" $st)
                           (message "Multi line region is not allowed")
                         $st)))
                    ((thing-at-point 'symbol)
                     (thing-at-point 'symbol))
                    (t ""))
              :prompt "Swoop: " ;; Don't change due to helm-swoop-caret-match
              :preselect
              ;; Get current line has content or else near one
              (if (string-match "^[\t\n\s]*$" $line)
                  (save-excursion
                    (if (re-search-forward "[^\t\n\s]" nil t)
                        (format "^%s\s" (line-number-at-pos))
                      (re-search-backward "[^\t\n\s]" nil t)
                      (format "^%s\s" (line-number-at-pos))))
                (format "^%s\s" (line-number-at-pos)))
              :candidate-number-limit 19999))
    ;; Restore helm's hook and window function etc
    (progn
      (remove-hook 'helm-move-selection-after-hook
                   'helm-swoop--synchronizing-position)
      (remove-hook 'helm-update-hook
                   'helm-swoop--pattern-match)
      (setq helm-display-function helm-swoop-display-tmp)
      (setq helm-swoop-first-position nil)
      (setq helm-swoop-last-query helm-pattern)
      (delete-overlay helm-swoop-line-overlay)
      ;; Scroll helm buffer
      (setq helm-display-source-at-screen-top helm-swoop-at-screen-top)
      (setq helm-completion-window-scroll-margin
            helm-swoop-store-scroll-margin)
      (helm-swoop-delete-overlay)
      (deactivate-mark t))))

;; Employ word from isearch ---------------
(defun helm-swoop-from-isearch ()
  "Invoke `helm-swoop' from isearch."
  (interactive)
  (let (($input (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (helm-swoop 0 $input)))
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; For helm-resume ------------------------
(defadvice helm-resume-select-buffer
  (around helm-swoop-if-selected-as-resume activate)
  "Resume if *Helm Swoop* buffer selected as a resume
 when helm-resume with prefix"
  (if (boundp 'helm-swoop-last-query)
      ad-do-it
    ;; When the buffer never call helm-swoop, just hide from options
    (let ((helm-buffers (delete "*Helm Swoop*" helm-buffers)))
      ad-do-it))
  (when (and (equal ad-return-value "*Helm Swoop*")
             (boundp 'helm-swoop-last-query))
    (helm-swoop helm-swoop-last-prefix-number helm-swoop-last-query)
    (setq ad-return-value nil)))

(defadvice helm-resume (around helm-swoop-resume activate)
  "Resume if the last used helm buffer is *Helm Swoop*"
  (if (equal helm-last-buffer "*Helm Swoop*") ;; 1
      (if (boundp 'helm-swoop-last-query)  ;; 2
          (if (not (ad-get-arg 0)) ;; 3
              (helm-swoop helm-swoop-last-prefix-number helm-swoop-last-query))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers))) ad-do-it)) ;; 2 else
    ad-do-it) ;; 1 else
    )

;; For caret beginning-match -----------------------------
(defun helm-swoop-caret-match-delete ($o $aft $beg $end &optional $len)
  (if $aft
      (- $end $beg $len) ;; Unused argument? to avoid byte compile error
    (delete-region (overlay-start $o) (1- (overlay-end $o)))))

(defun helm-swoop-caret-match (&optional $resume)
  (interactive)
  (if (and (string-match "^Swoop\\:\s" (buffer-substring-no-properties
                                     (point-min) (point-max)) )
           (eq (point) 8))
      (progn
        (if $resume
            (insert $resume) ;; NEED FIX #1 to appear as a "^"
          (insert "^[0-9]+."))
        (goto-char (point-min))
        (re-search-forward "^Swoop\\:\s\\(\\^\\[0\\-9\\]\\+\\.\\)" nil t)
        (let (($o (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put $o 'face 'helm-swoop-target-word-face)
          (overlay-put $o 'modification-hooks '(helm-swoop-caret-match-delete))
          (overlay-put $o 'display "^")
          (overlay-put $o 'evaporate t)))
    (if (minibufferp) (insert "^"))))

(unless (featurep 'helm-migemo)
  (define-key helm-map (kbd "^") 'helm-swoop-caret-match))

(provide 'helm-swoop)
;;; helm-swoop.el ends here
