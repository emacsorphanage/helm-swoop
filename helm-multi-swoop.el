;;; helm-multi-swoop.el --- Efficiently hopping squeezed lines powered by helm interface -*- coding: utf-8; lexical-binding: t -*-

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
(require 'helm-multi-swoop-edit)

(defvar helm-multi-swoop-buffer-list "*helm-multi-swoop buffers list*"
  "Buffer name")
(defvar helm-multi-swoop-ignore-buffers-match "^\\*"
  "Regexp to eliminate buffers you don't want to see")
(defvar helm-multi-swoop-candidate-number-limit 250)
(defvar helm-multi-swoop-last-selected-buffers nil)
(defvar helm-multi-swoop-last-query nil)
(defvar helm-multi-swoop-query nil)
(defvar helm-multi-swoop-buffer "*Helm Multi Swoop*")

(defvar helm-multi-swoop-map
  (let (($map (make-sparse-keymap)))
    (set-keymap-parent $map helm-map)
    (define-key $map (kbd "C-c C-e") 'helm-multi-swoop-edit)
    (delq nil $map)))

(defvar helm-multi-swoop-buffers-map
  (let (($map (make-sparse-keymap)))
    (set-keymap-parent $map helm-map)
    (define-key $map (kbd "RET")
      (lambda () (interactive)
        (helm-quit-and-execute-action 'helm-multi-swoop--exec)))
    (delq nil $map)))

;; action -----------------------------------------------------

(defadvice helm-next-line (around helm-multi-swoop-next-line disable)
  ad-do-it
  (when (called-interactively-p 'any)
    (helm-multi-swoop--move-line-action)))

(defadvice helm-previous-line (around helm-multi-swoop-previous-line disable)
  ad-do-it
  (when (called-interactively-p 'any)
    (helm-multi-swoop--move-line-action)))

(defun helm-multi-swoop--move-line-action ()
  (with-helm-window
    (let* (($key (buffer-substring (point-at-bol) (point-at-eol)))
           ($num (when (string-match "^[0-9]+" $key)
                   (string-to-number (match-string 0 $key))))
           ($buf (get-text-property 0 'buffer-name $key)))
      ;; Synchronizing line position
      (with-selected-window helm-swoop-synchronizing-window
        (when (not (equal $buf ""))
          (switch-to-buffer $buf)
          (helm-swoop--pattern-match)
          (if $num
              (helm-swoop--goto-line $num)
            (goto-char (point-min))))
        ;;(delete-overlay helm-swoop-line-overlay)
        (move-overlay helm-swoop-line-overlay
                      (goto-char (point-at-bol))
                      (save-excursion
                        (goto-char (point-at-bol))
                        (or (search-forward "\n" nil t)
                            (point-max)))
                      (get-buffer $buf))
        (recenter)))))

(defun helm-multi-swoop--get-marked-buffers ()
  (let ($list)
    (with-current-buffer helm-multi-swoop-buffer-list
      (dolist ($ov (overlays-in (point-min) (point-max)))
        (when (eq 'helm-visible-mark (overlay-get $ov 'face))
          (setq $list (cons
                       (let (($word (buffer-substring-no-properties
                                     (overlay-start $ov) (overlay-end $ov))))
                         (mapcar (lambda ($r)
                                   (setq $word (replace-regexp-in-string
                                                (car $r) (cdr $r) $word)))
                                 (list '("\\`[ \t\n\r]+" . "")
                                       '("[ \t\n\r]+\\'" . "")))
                         $word)
                       $list)))))
    (delete "" $list)))

;; temporary override ------------------------------------------

(defun helm-multi-swoop--scrolling-set ()
  ;; Modify scrolling temporarily
  (when helm-display-source-at-screen-top
    (setq helm-display-source-at-screen-top nil))
  (setq helm-completion-window-scroll-margin 6))

(defun helm-multi-swoop--scrolling-reset ()
  (setq helm-display-source-at-screen-top helm-swoop-at-screen-top)
  (setq helm-completion-window-scroll-margin helm-swoop-store-scroll-margin))

;; core --------------------------------------------------------

(defun helm-multi-swoop--exec ($candidate &optional $query $buffer-list)
  (interactive)
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point (cons (point) (buffer-name (current-buffer))))
  (let (($buffs (or $buffer-list (helm-multi-swoop--get-marked-buffers)))
        $contents)
    (setq helm-multi-swoop-last-selected-buffers $buffs)
    ;; Create buffer sources
    (mapc (lambda ($x)
            (with-current-buffer $x
              (let* (($buf (buffer-name (current-buffer)))
                     ($cont (concat (helm-swoop--get-content) "\n")))
                (setq $cont (propertize $cont 'buffer-name $buf))
                (setq
                 $contents
                 (cons
                  `((name . ,$buf)
                    (candidates . (lambda () (split-string ,$cont "\n")))
                    (action . (lambda ($line)
                                (switch-to-buffer ,$buf)
                                (helm-swoop--goto-line
                                 (when (string-match "^[0-9]+" $line)
                                   (string-to-number
                                    (match-string 0 $line))))
                                (when (re-search-forward
                                       (mapconcat 'identity
                                                  (split-string
                                                   helm-pattern " ") "\\|")
                                       nil t)
                                  (goto-char (match-beginning 0)))
                                (recenter)))
                    (header-line . "[C-c C-e] Edit mode")
                    (keymap . ,helm-multi-swoop-map))
                  $contents)))))
          $buffs)

    (unwind-protect
        (progn
          (ad-enable-advice 'helm-next-line 'around
                            'helm-multi-swoop-next-line)
          (ad-activate 'helm-next-line)
          (ad-enable-advice 'helm-previous-line 'around
                            'helm-multi-swoop-previous-line)
          (ad-activate 'helm-previous-line)
          (add-hook 'helm-update-hook 'helm-swoop--pattern-match)
          (helm-multi-swoop--scrolling-set)
          (setq helm-swoop-line-overlay
                (make-overlay (point) (point)))
          (overlay-put helm-swoop-line-overlay
                       'face 'helm-swoop-target-line-face)
          (setq helm-display-function helm-swoop-split-window-function)
          (unless (boundp 'helm-swoop-last-query)
            (set (make-local-variable 'helm-swoop-last-query) ""))
          ;; Execute helm
          (helm :sources $contents
                :buffer helm-multi-swoop-buffer
                :input (or $query helm-multi-swoop-query "")
                :prompt helm-swoop-prompt
                :candidate-number-limit
                helm-multi-swoop-candidate-number-limit))
      ;; Restore
      (progn
        (ad-disable-advice 'helm-next-line 'around
                           'helm-multi-swoop-next-line)
        (ad-activate 'helm-next-line)
        (ad-disable-advice 'helm-previous-line 'around
                           'helm-multi-swoop-previous-line)
        (ad-activate 'helm-previous-line)
        (remove-hook 'helm-update-hook 'helm-swoop--pattern-match)
        (helm-swoop--scrolling-reset)
        (setq helm-display-function helm-swoop-display-tmp)
        (setq helm-multi-swoop-last-query helm-pattern)
        (helm-multi-swoop--scrolling-reset)
        (setq helm-multi-swoop-query nil)
        ;; Delete overlay
        (mapc (lambda ($x)
                (with-current-buffer $x
                  (delete-overlay helm-swoop-line-overlay)
                  (helm-swoop--delete-overlay 'target-buffer)))
              $buffs)))))

(defun helm-multi-swoop--get-buffer-list ()
  (let ($buflist1 $buflist2)
    ;; eliminate buffers start with whitespace
    (mapcar (lambda ($buf)
              (setq $buf (buffer-name $buf))
              (unless (string-match "^\\s-" $buf)
                (setq $buflist1 (cons $buf $buflist1))))
            (buffer-list))
    ;; eliminate buffers match pattern
    (mapcar (lambda ($buf)
              (unless (string-match
                       helm-multi-swoop-ignore-buffers-match
                       $buf)
                (setq $buflist2 (cons $buf $buflist2))))
            $buflist1)
    $buflist2))

(defun helm-c-source-helm-multi-swoop-buffers ()
  "Show buffer list to select"
  `((name . "helm-multi-swoop select buffers")
    (candidates . helm-multi-swoop--get-buffer-list)
    (header-line . "[C-SPC]/[M-SPC] select, [RET] next step")
    (keymap . ,helm-multi-swoop-buffers-map)))

;;;###autoload
(defun helm-multi-swoop (&optional $query $buffer-list)
  (interactive)
  "\
Usage:
M-x helm-multi-swoop
1. Select any buffers by [C-SPC] or [M-SPC]
2. Press [RET] to start helm-multi-swoop

C-u M-x helm-multi-swoop
If you have done helm-multi-swoop before, you can skip select buffers step.
Last selected buffers will be applied to helm-multi-swoop.
"
  (cond ($query
         (setq helm-multi-swoop-query $query))
        (mark-active
         (let (($st (buffer-substring-no-properties
                     (region-beginning) (region-end))))
           (if (string-match "\n" $st)
               (message "Multi line region is not allowed")
             (setq helm-multi-swoop-query $st))))
        ((helm-swoop--thing-at-point)
         (setq helm-multi-swoop-query (helm-swoop--thing-at-point)))
        (t (setq helm-multi-swoop-query "")))
  (if (equal current-prefix-arg '(4))
      (helm-multi-swoop--exec nil helm-multi-swoop-query $buffer-list)
    (if $buffer-list
        (helm-multi-swoop--exec nil $query $buffer-list)
      (helm :sources (helm-c-source-helm-multi-swoop-buffers)
            :buffer helm-multi-swoop-buffer-list
            :prompt "Mark any buffers by [C-SPC] or [M-SPC]: "))))

(defun helm-multi-swoop-all (&optional $query)
  (interactive)
  "Apply all buffers"
  (cond ($query
         (setq helm-multi-swoop-query $query))
        (mark-active
         (let (($st (buffer-substring-no-properties
                     (region-beginning) (region-end))))
           (if (string-match "\n" $st)
               (message "Multi line region is not allowed")
             (setq helm-multi-swoop-query $st))))
        ((helm-swoop--thing-at-point)
         (setq helm-multi-swoop-query (helm-swoop--thing-at-point)))
        (t (setq helm-multi-swoop-query "")))
  (helm-multi-swoop--exec nil
                          helm-multi-swoop-query
                          (helm-multi-swoop--get-buffer-list)))

;; option -------------------------------------------------------

(defun helm-multi-swoop-all-from-isearch ()
  "Invoke `helm-swoop' from isearch."
  (interactive)
  (let (($input (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (helm-multi-swoop-all $input)))
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "C-x M-i") 'helm-multi-swoop-all-from-isearch)

(defadvice helm-resume (around helm-multi-swoop-resume activate)
  "Resume if the last used helm buffer is *Helm Swoop*"
  (if (equal helm-last-buffer helm-multi-swoop-buffer)

      (if (boundp 'helm-multi-swoop-last-query)
          (if (not (ad-get-arg 0))
              (helm-multi-swoop helm-multi-swoop-last-query
                                helm-multi-swoop-last-selected-buffers))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers))) ad-do-it))
    ad-do-it))

(provide 'helm-multi-swoop)
;;; helm-swoop.el ends here
