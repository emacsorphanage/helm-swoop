;;; helm-multi-swoop-edit.el --- Efficiently hopping squeezed lines powered by helm interface -*- coding: utf-8; lexical-binding: t -*-

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

(defvar helm-multi-swoop-edit-save t
  "Save each buffer you edit when editing is complete")
(defvar helm-multi-swoop-edit-buffer "*Helm Multi Swoop Edit*")

(defvar helm-multi-swoop-edit-map
  (let (($map (make-sparse-keymap)))
    (define-key $map (kbd "C-x C-s") 'helm-multi-swoop--edit-complete)
    (define-key $map (kbd "C-c C-g") 'helm-multi-swoop--edit-cancel)
    $map))

(defun helm-multi-swoop--edit ($candidate)
  "This function will only be called from `helm-swoop-edit'"
  (interactive)
  (helm-swoop--delete-overlay 'target-buffer)
  (with-current-buffer (get-buffer-create helm-multi-swoop-edit-buffer)
    (helm-swoop--clear-edit-buffer 'helm-multi-swoop-edit)
    (let (($bufstr "") ($mark nil))
      ;; Get target line number to edit
      (with-current-buffer helm-multi-swoop-buffer
        ;; Set overlay to helm-source-header for editing marked lines
        (save-excursion
          (goto-char (point-min))
          (let (($beg (point)) $end)
            (while (setq $beg (text-property-any $beg (point-max)
                                              'face 'helm-source-header))
              (setq $end (next-single-property-change $beg 'face))
              (overlay-put (make-overlay $beg $end) 'source-header t)
              (setq $beg $end)
              (goto-char $end))))
        ;; Use selected line by [C-SPC] or [M-SPC]
        (dolist ($ov (overlays-in (point-min) (point-max)))
          (when (overlay-get $ov 'source-header)
            (setq $bufstr (concat (buffer-substring
                                   (overlay-start $ov) (overlay-end $ov))
                                  $bufstr)))
          (when (eq 'helm-visible-mark (overlay-get $ov 'face))
            (let (($str (buffer-substring (overlay-start $ov) (overlay-end $ov))))
              (unless (equal "" $str) (setq $mark t))
              (setq $bufstr (concat (buffer-substring
                                     (overlay-start $ov) (overlay-end $ov))
                                    $bufstr)))))
        (if $mark
            (progn (setq $bufstr (concat "Helm Multi Swoop\n" $bufstr))
                   (setq $mark nil))
          (setq $bufstr (concat "Helm Multi Swoop\n"
                                (buffer-substring
                                 (point-min) (point-max))))))

      ;; Set for edit buffer
      (insert $bufstr)
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t front-sticky t))

      ;; Set for editable context
      (let ((inhibit-read-only t))
        ;; Title and explanation
        (goto-char (point-min))
        (let (($o (make-overlay (point) (point-at-eol))))
          (overlay-put $o 'helm-multi-swoop-edit t)
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
  (switch-to-buffer helm-multi-swoop-edit-buffer)
  (goto-char (point-min))
  (if (string-match "^[0-9]+" $candidate)
      (re-search-forward
       (concat "^" (match-string 0 $candidate)) nil t))
  (use-local-map helm-multi-swoop-edit-map))

(defun helm-multi-swoop--separate-text-property-into-list ($property)
  (interactive)
  (let ($list $end)
    (save-excursion
      (goto-char (point-min))
      (while (setq $end (next-single-property-change (point) $property))
        (setq $list (cons (buffer-substring-no-properties (point) $end)
                          $list))
        (goto-char $end))
      (setq $list (cons (buffer-substring-no-properties (point) (point-max))
                        $list)))
    (nreverse $list)))

(defun helm-multi-swoop--collect-edited-lines ()
  "Create a list of edited lines with each its own line number"
  (interactive)
  (let* (($list
          (helm-multi-swoop--separate-text-property-into-list 'helm-header))
         ($length (length $list))
         ($i 1) ;; 0th $list is header
         $pairs)
    (while (<= $i $length)
      (let ($contents)
        ;; Make ((number . line) (number . line) (number . line) ...)
        (with-temp-buffer
         (insert (format "%s" (nth (1+ $i) $list)))
         (goto-char (point-min))
         (while (re-search-forward "^\\([0-9]+\\)\s" nil t)
           (setq $contents
                 (cons (cons (string-to-number (match-string 1))
                             (buffer-substring-no-properties
                              (point)
                              (save-excursion
                                (if (re-search-forward
                                     "^\\([0-9]+\\)\s\\|^\\(\\-+\\)" nil t)
                                    (1- (match-beginning 0))
                                  (goto-char (point-max))
                                  (re-search-backward "\n" nil t)))))
                       $contents))))
        ;; Make ((buffer-name (number . line) (number . line) ...)
        ;;       (buffer-name (number . line) (number . line) ...) ...)
        (setq $pairs (cons (cons (nth $i $list) $contents) $pairs)))
      (setq $i (+ $i 2)))
    (delete '(nil) $pairs)))

(defun helm-multi-swoop--edit-complete ()
  "Apply changes to buffers and kill temporary edit buffer"
  (interactive)
  (let (($list (helm-multi-swoop--collect-edited-lines)))
    (mapc (lambda ($x)
            (with-current-buffer (car $x)
              (save-excursion
                (loop for ($k . $v) in (cdr $x)
                      do (progn
                           (goto-char (point-min))
                           (delete-region (point-at-bol $k) (point-at-eol $k))
                           (goto-char (point-at-bol $k))
                           (insert $v))))
              (if helm-multi-swoop-edit-save
                  (save-buffer))))
          $list)
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer helm-multi-swoop-edit-buffer)))
  (message "Successfully helm-multi-swoop-edit applied to original buffer"))

(defun helm-multi-swoop--edit-cancel ()
  "Cancel edit and kill temporary buffer"
  (interactive)
  (select-window helm-swoop-synchronizing-window)
  (kill-buffer (get-buffer helm-multi-swoop-edit-buffer))
  (message "helm-multi-swoop-edit canceled"))

;;;###autoload
(defun helm-multi-swoop-edit ()
  (interactive)
  (helm-quit-and-execute-action 'helm-multi-swoop--edit))

(provide 'helm-multi-swoop-edit)
;;; helm-swoop.el ends here
