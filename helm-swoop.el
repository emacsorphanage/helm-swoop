;;; helm-swoop.el --- Efficiently hopping squeezed lines powered by helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013 by Shingo Fukuyama

;; Version: 1.0
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

;;; Code:

(eval-when-compile (require 'cl))

(require 'helm)

(defvar helm-swoop-target-line-face
  '((foreground-color . "#333333")
    (background-color . "#ffff00")))

(defvar helm-swoop-target-word-face
  '((foreground-color . "#ffffff")
    (background-color . "#7700ff")))

(defvar helm-swoop-split-window-function
  (lambda ($buf)
    (when (one-window-p)
      ;;(split-window-horizontally)
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer $buf))
  "Change the way to split window only when `helm-swoop' is calling")

(defvar helm-swoop-first-position nil
  "For keep line position when `helm-swoop' is called")

;; Avoid compile error for apply buffer local variable
(defvar helm-swoop-cache)
(defvar helm-swoop-last-point)

(defvar helm-swoop-synchronizing-window nil
  "Window object where `helm-swoop' called from")
(defvar helm-swoop-target-buffer nil
  "Buffer object where `helm-swoop' called from")
(defvar helm-swoop-overlay nil
  "Overlay object to indicates other window's line")

(defun helm-swoop-back-to-last-point ()
  (interactive)
  "Go back to last position where `helm-swoop' was called"
  (if (and (boundp helm-swoop-last-point)
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
    (delete-overlay $o)))

(defun helm-swoop-get-string-at-line ()
  "Get string at the line. 10-20% firster than (thing-at-point 'line)"
  (buffer-substring-no-properties
 (point-at-bol) (point-at-eol)))

(defun helm-swoop-target-line-overlay ()
  "Add color to the target line"
  (overlay-put (setq helm-swoop-overlay
                     (make-overlay (point-at-bol) (point-at-eol)))
               'face helm-swoop-target-line-face))

;; core ------------------------------------------------

(defun helm-swoop-synchronizing-position ()
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
                (delete-overlay helm-swoop-overlay)
                (helm-swoop-target-line-overlay))
              (recenter))
          (move-beginning-of-line 1)
          (helm-swoop-target-line-overlay)
          (recenter)
          (setq helm-swoop-first-position t)))
      )))

(defun helm-swoop-pattern-match ()
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
                  (while (re-search-forward $wd nil t)
                    (setq $o (make-overlay (match-beginning 0) (match-end 0)))
                    (overlay-put $o 'face helm-swoop-target-word-face)))))
            )))))

(defun helm-swoop-list ()
  "Get the all lines in buffer into list"
  (let ($list $line)
    (save-excursion
      (goto-char (point-min))
      (setq $list
            (loop until (eobp)
                  if (not (string-match
                           "^[\t\n\s]*$"
                           (setq $line (helm-swoop-get-string-at-line))))
                  collect (format "%s %s" (line-number-at-pos) $line)
                  do (forward-line 1))))
    $list))

(defun helm-c-source-swoop ($list)
  `((name . "Helm Swoop")
    (candidates . ,$list)
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
    (migemo)))

(defvar helm-swoop-display-tmp helm-display-function
  "To restore helm window display function")

;; Delete cache when modified file is saved
(add-hook
 'after-save-hook (lambda ()
                    (if helm-swoop-cache
                        (setq helm-swoop-cache nil))))

;;;###autoload
(defun helm-swoop ()
  (interactive)
  "List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list."
  (setq helm-swoop-synchronizing-window (selected-window))
  (if (boundp 'helm-swoop-last-point)
      (setq helm-swoop-last-point (point))
    (set (make-local-variable 'helm-swoop-last-point) nil)
    (setq helm-swoop-last-point (point)))
  (setq helm-swoop-last-point (point))
  (setq helm-swoop-target-buffer (current-buffer))
  (setq helm-swoop-overlay (make-overlay (point-at-bol) (point-at-eol)))
  ;; Cache
  (cond ((not (boundp 'helm-swoop-cache))
         (set (make-local-variable 'helm-swoop-cache) (helm-swoop-list)))
        ((not helm-swoop-cache)
         (setq helm-swoop-cache (helm-swoop-list)))
        ((buffer-modified-p)
         (setq helm-swoop-cache (helm-swoop-list))))
  (unwind-protect
      (let (($line (helm-swoop-get-string-at-line)))
        ;; Modify window split function temporary
        (setq helm-display-function helm-swoop-split-window-function)
        ;; For synchronizing line position
        (add-hook 'helm-move-selection-after-hook
                  'helm-swoop-synchronizing-position)
        (add-hook 'helm-update-hook
                  'helm-swoop-pattern-match)
        ;; Execute helm
        (helm :sources (helm-c-source-swoop helm-swoop-cache)
              :buffer "*Helm Swoop*"
              :input
              (if mark-active
                  (let (($st (buffer-substring-no-properties
                              (region-beginning) (region-end))))
                  (if (string-match "\n" $st)
                      (message "Multi line region is not allowed")
                    $st))
                "")
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
    ;; Restore helm's hook and window function
    (progn
      (remove-hook 'helm-move-selection-after-hook
                   'helm-swoop-synchronizing-position)
      (remove-hook 'helm-update-hook
                   'helm-swoop-pattern-match)
      (setq helm-display-function helm-swoop-display-tmp)
      (setq helm-swoop-first-position nil)
      (delete-overlay helm-swoop-overlay)
      (helm-swoop-delete-overlay)
      (deactivate-mark t)
      )))

(provide 'helm-swoop)
;;; helm-swoop.el ends here
