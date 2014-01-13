;;; helm-swoop-edit.el --- Efficiently hopping squeezed lines powered by helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013 by Shingo Fukuyama

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

(defvar helm-swoop-edit-target-buffer)
(defvar helm-swoop-edit-buffer "*Helm Swoop Edit*")

(defvar helm-swoop-edit-map
  (let (($map (make-sparse-keymap)))
    (define-key $map (kbd "C-x C-s") 'helm-swoop--edit-complete)
    (define-key $map (kbd "C-c C-g") 'helm-swoop--edit-cancel)
    $map))

(defun helm-swoop--clear-edit-buffer ($prop)
  (let ((inhibit-read-only t))
    (mapc (lambda ($ov)
            (when (overlay-get $ov $prop)
              (delete-overlay $ov)))
          (overlays-in (point-min) (point-max)))
    (set-text-properties (point-min) (point-max) nil)
    (goto-char (point-min))
    (erase-buffer)))

(defun helm-swoop--collect-edited-lines ()
  "Create a list of edited lines with each its own line number"
  (interactive)
  (let ($list)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\)\s" nil t)
      (setq $list
            (cons (cons (string-to-number (match-string 1))
                        (buffer-substring-no-properties
                         (point)
                         (save-excursion
                           (if (re-search-forward
                                "^\\([0-9]+\\)\s\\|^\\(\\-+\\)" nil t)
                               (1- (match-beginning 0))
                             (goto-char (point-max))
                             (re-search-backward "\n" nil t)))))
                  $list)))
    $list))

(defun helm-swoop--edit ($candidate)
  "This function will only be called from `helm-swoop-edit'"
  (interactive)
  (setq helm-swoop-edit-target-buffer helm-swoop-target-buffer)
  (helm-swoop--delete-overlay 'target-buffer)
  (with-current-buffer (get-buffer-create helm-swoop-edit-buffer)

    (helm-swoop--clear-edit-buffer 'helm-swoop-edit)
    (let (($bufstr ""))
      ;; Get target line number to edit
      (with-current-buffer helm-swoop-buffer
        ;; Use selected line by [C-SPC] or [M-SPC]
        (mapc (lambda ($ov)
                (when (eq 'helm-visible-mark (overlay-get $ov 'face))
                  (setq $bufstr (concat (buffer-substring-no-properties
                                         (overlay-start $ov) (overlay-end $ov))
                                        $bufstr))))
              (overlays-in (point-min) (point-max)))
        (if (equal "" $bufstr)
            ;; Not found selected line
            (setq $bufstr (buffer-substring-no-properties
                           (point-min) (point-max)))
          ;; Attach title
          (setq $bufstr (concat "Helm Swoop\n" $bufstr))))

      ;; Set for edit buffer
      (insert $bufstr)
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t front-sticky t))

      ;; Set for editable context
      (let ((inhibit-read-only t))
        ;; Title and explanation
        (goto-char (point-min))
        (let (($o (make-overlay (point) (point-at-eol))))
          (overlay-put $o 'helm-swoop-edit t)
          (overlay-put $o 'face 'font-lock-function-name-face)
          (overlay-put $o 'after-string
                       (propertize " [C-x C-s] Complete, [C-c C-g] Cancel"
                                   'face 'helm-bookmark-addressbook)))
        ;; Line number and editable area
        (while (re-search-forward "^\\([0-9]+\s\\)\\(.*\\)$" nil t)
          (let* (($bol1 (match-beginning 1))
                 ($eol1 (match-end 1))
                 ($bol2 (match-beginning 2))
                 ($eol2 (match-end 2)))

            ;; Line number
            (add-text-properties $bol1 $eol1
                                 '(face font-lock-function-name-face
                                   intangible t))
            ;; Editable area
            (remove-text-properties $bol2 $eol2 '(read-only t))
            ;; (add-text-properties $bol2 $eol2 '(font-lock-face helm-match))

            ;; For line tail
            (set-text-properties $eol2 (or (1+ $eol2) (point-max))
                                 '(read-only t rear-nonsticky t))))
        (helm-swoop--target-word-overlay 'edit-buffer 0))))

  (other-window 1)
  (switch-to-buffer helm-swoop-edit-buffer)
  (goto-char (point-min))
  (if (string-match "^[0-9]+" $candidate)
      (re-search-forward
       (concat "^" (match-string 0 $candidate)) nil t))
  (use-local-map helm-swoop-edit-map))

(defun helm-swoop--edit-complete ()
  "Apply changes and kill temporary edit buffer"
  (interactive)
  (let (($list (helm-swoop--collect-edited-lines)))
    (with-current-buffer helm-swoop-edit-target-buffer
      ;; Replace from the end of buffer
      (save-excursion
      (loop for ($k . $v) in $list
            do (progn
                 (goto-char (point-min))
                 (delete-region (point-at-bol $k) (point-at-eol $k))
                 (goto-char (point-at-bol $k))
                 (insert $v)))))
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer helm-swoop-edit-buffer)))
  (message "Successfully helm-swoop-edit applied to original buffer"))

(defun helm-swoop--edit-cancel ()
  "Cancel edit and kill temporary buffer"
  (interactive)
  (select-window helm-swoop-synchronizing-window)
  (kill-buffer (get-buffer helm-swoop-edit-buffer))
  (message "helm-swoop-edit canceled"))

(defun helm-swoop-edit ()
  (interactive)
  (helm-quit-and-execute-action 'helm-swoop--edit))

(provide 'helm-swoop-edit)
;;; helm-swoop.el ends here
