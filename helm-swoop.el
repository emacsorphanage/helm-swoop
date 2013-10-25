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

(defvar helm-swoop-split-window-function
  (lambda ($buf)
    (when (one-window-p)
      ;;(split-window-horizontally)
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer $buf))
  "Change the way to split window only when `helm-swoop' is calling")

(defvar helm-swoop-last-point nil
  "For jump back once")

(defvar helm-swoop-first-time nil
  "For keep line position when `helm-swoop' calls")

(defvar helm-swoop-synchronizing-window nil
  "Store window identity for synchronizing")

(defvar helm-swoop-overlay nil
  "Store overlay object")

(defvar helm-swoop-target-buffer nil
  "For overlay")

(defun helm-swoop-back-to-last-point ()
  (interactive)
  "Go back to last position where `helm-swoop' was called"
  (when helm-swoop-last-point
    (let (($po (point)))
      (goto-char helm-swoop-last-point)
      (setq helm-swoop-last-point $po))))

(defun helm-swoop-trim-whitespace ($s)
  "Return string without whitespace at the both beginning and end"
  (if (string-match "\\`\\(?:\\s-+\\)?\\(.+?\\)\\(?:\\s-+\\)?\\'" $s)
      (match-string 1 $s)
    $s))

(defun helm-swoop-get-string-at-line ()
  "Get string at the line. 10-20% firster than (thing-at-point 'line)"
  (buffer-substring-no-properties
 (point-at-bol) (point-at-eol)))

(defun helm-swoop-target-overlay ()
  "Add color to target line"
  (overlay-put (setq helm-swoop-overlay
                     (make-overlay (point-at-bol) (point-at-eol)))
               'face helm-swoop-target-line-face))

(defun helm-swoop-synchronizing-position ()
  (with-helm-window
    (let* (($key (helm-swoop-trim-whitespace
                  (helm-swoop-get-string-at-line)))
           ($cand (assoc-default 'candidates (helm-get-current-source)))
           ($prop (assoc-default $key $cand)))
      ;; Synchronizing line position
      (with-selected-window helm-swoop-synchronizing-window
        (if helm-swoop-first-time
            (progn
              (goto-line $prop)
              (with-current-buffer helm-swoop-target-buffer
                (delete-overlay helm-swoop-overlay)
                (helm-swoop-target-overlay))
              (recenter))
          (move-beginning-of-line 1)
          (helm-swoop-target-overlay)
          (recenter)
          (setq helm-swoop-first-time t)))
      )))

(defun helm-swoop-list ()
  "Get the all lines in buffer into list"
  (let ($list $line $pos)
    (save-excursion
      (goto-char (point-min))
      (setq $list
            (loop until (eobp)
                  if (not (string-match
                           "^[\t\n\s]*$"
                           (setq $line (helm-swoop-get-string-at-line))))
                  collect (cons
                           (helm-swoop-trim-whitespace
                            (concat
                             (format "%s" (setq $pos (line-number-at-pos)))
                             " "
                             $line))
                           $pos)
                  do (forward-line 1))))
    $list))

(defun helm-c-source-swoop ($list)
  `((name . "Helm Swoop")
    (candidates . ,$list)
    (candidatees-in-buffer)
    (action . (lambda ($po) (goto-line $po) (recenter))))
  )


(defvar helm-swoop-display-tmp helm-display-function
  "To restore helm window display function")

(defvar-local helm-swoop-cache nil
  "If buffer is not modified, cache is used")

;;;###autoload
(defun helm-swoop ()
  (interactive)
  "List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list."
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point (point))
  (setq helm-swoop-target-buffer (current-buffer))
  (setq helm-swoop-overlay (make-overlay (point-at-bol) (point-at-eol)))
  ;; Cache
  (cond ((not helm-swoop-cache)
         (setq helm-swoop-cache (helm-swoop-list)))
        ((buffer-modified-p)
         (setq helm-swoop-cache (helm-swoop-list))))
  (unwind-protect
      (let (($list (helm-swoop-list))
            ($line (helm-swoop-get-string-at-line)))
        ;; Modify window split function temporary
        (setq helm-display-function helm-swoop-split-window-function)
        ;; For synchronizing line position
        (add-hook 'helm-move-selection-after-hook
                  'helm-swoop-synchronizing-position)
        ;; Execute helm
        (helm :sources (helm-c-source-swoop $list)
              :buffer "*Helm Swoop*"
              :preselect
              ;; get current line has content or else near one
              (if (string-match "^[\t\n\s]*$" $line)
                  (save-excursion
                    (if (re-search-forward "[^\t\n\s]" nil t)
                        (format "^%s" (line-number-at-pos))
                      (re-search-backward "[^\t\n\s]" nil t)
                      (format "^%s" (line-number-at-pos))))
                (format "^%s" (line-number-at-pos)))
              :candidate-number-limit 999))
    ;; Restore helm's hook and window function
    (progn
      (remove-hook 'helm-move-selection-after-hook
                   'helm-swoop-synchronizing-position)
      (setq helm-display-function helm-swoop-display-tmp)
      (setq helm-swoop-first-time nil)
      (delete-overlay helm-swoop-overlay)
      )))

(provide 'helm-swoop)
;;; helm-swoop.el ends here
