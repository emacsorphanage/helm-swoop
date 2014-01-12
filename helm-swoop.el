;;; helm-swoop.el --- Efficiently hopping squeezed lines powered by helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013 by Shingo Fukuyama

;; Version: 1.3
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
;; (require 'helm)

;; ;; Locate the helm-swoop folder to your path
;; ;; This line is unnecessary if you get this program from MELPA
;; (add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")

;; (require 'helm-swoop)

;; ;; Change keybinds to whatever you like :)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; ;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; ;; Save buffer when helm-multi-swoop-edit complete
;; (setq helm-multi-swoop-edit-save t)

;; ;; If this value is t, split window inside the current window
;; (setq helm-swoop-split-with-multiple-windows nil)

;; ;; Split direction. 'split-window-vertically or 'split-window-horizontally
;; (setq helm-swoop-split-direction 'split-window-vertically)

;; ;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color t)
;; ----------------------------------------------------------------

;; Helm Swoop Edit
;; While doing helm-swoop, press keybind [C-c C-e] to move to edit buffer.
;; Edit the list and apply by [C-x C-s]. If you'd like to cancel, [C-c C-g]

;;; Code:

(eval-when-compile (require 'cl))

(require 'helm)
(require 'helm-swoop-edit)
(require 'helm-multi-swoop)

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

(defcustom helm-swoop-speed-or-color t
 "If nil, you can slightly boost invoke speed in exchange for text color"
 :group 'helm-swoop
 :type 'boolean)

(defcustom helm-swoop-split-with-multiple-windows nil
 "Split window when having multiple windows open"
 :group 'helm-swoop
 :type 'boolean)

(defcustom helm-swoop-split-direction 'split-window-vertically
 "Split direction"
 :type '(choice (const :tag "vertically"   split-window-vertically)
                (const :tag "horizontally" split-window-horizontally))
 :group 'helm-swoop)

(defvar helm-swoop-split-window-function
  (lambda ($buf)
   (if helm-swoop-split-with-multiple-windows
       (funcall helm-swoop-split-direction)
       (when (one-window-p)
        (funcall helm-swoop-split-direction)))
    (other-window 1)
    (switch-to-buffer $buf))
  "Change the way to split window only when `helm-swoop' is calling")
(defvar helm-swoop-at-screen-top helm-display-source-at-screen-top)
(defvar helm-swoop-store-scroll-margin helm-completion-window-scroll-margin)
(defvar helm-swoop-candidate-number-limit 19999)
(defvar helm-swoop-buffer "*Helm Swoop*")
(defvar helm-swoop-prompt "Swoop: ")
(defvar helm-swoop-last-point nil)

;; Buffer local variables
(defvar helm-swoop-cache)
(defvar helm-swoop-list-cache)
(defvar helm-swoop-last-query)         ; Last search query for resume
(defvar helm-swoop-last-prefix-number) ; For multiline highlight

;; Global variables
(defvar helm-swoop-synchronizing-window nil
  "Window object where `helm-swoop' called from")
(defvar helm-swoop-target-buffer nil
  "Buffer object where `helm-swoop' called from")
(defvar helm-swoop-line-overlay nil
  "Overlay object to indicate other window's line")

(defsubst helm-swoop--goto-line ($line)
  (goto-char (point-min))
  (unless (search-forward "\n" nil t (1- $line))
    (goto-char (point-max))))

(defsubst helm-swoop--delete-overlay ($identity &optional $beg $end)
  (or $beg (setq $beg (point-min)))
  (or $end (setq $end (point-max)))
  (dolist ($o (overlays-in $beg $end))
    (if (overlay-get $o $identity)
        (delete-overlay $o))))

(defsubst helm-swoop--get-string-at-line ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defsubst helm-swoop--thing-at-point ()
  "For fix wrong-type-argument stringp error"
  (substring-no-properties (or (thing-at-point 'symbol) "")))

;;;###autoload
(defun helm-swoop-back-to-last-point (&optional $cancel)
  (interactive)
  "Go back to last position where `helm-swoop' was called"
  (if helm-swoop-last-point
      (let (($po (point)))
        (switch-to-buffer (cdr helm-swoop-last-point))
        (goto-char (car helm-swoop-last-point))
        (unless $cancel
          (setq helm-swoop-last-point
                (cons $po (buffer-name (current-buffer))))))))

(defun helm-swoop--split-lines-by ($string $regexp $step)
  "split-string by $step for multiline"
  (or $step (setq $step 1))
  (let (($from1 0) ;; last match point
        ($from2 0) ;; last substring point
        $list
        ($i 1)) ;; from line 1
    (while (string-match $regexp $string $from1)
      (setq $i (1+ $i))
      (if (eq 0 (% $i $step))
          (progn
            (setq $list (cons (substring $string $from2 (match-beginning 0))
                              $list))
            (setq $from2 (match-end 0))
            (setq $from1 (match-end 0)))
        (setq $from1 (match-end 0))))
    (setq $list (cons (substring $string $from2) $list))
    (nreverse $list)))

(defun helm-swoop--target-line-overlay-move ()
  "Add color to the target line"
  (move-overlay
   helm-swoop-line-overlay
   (progn
     (search-backward
      "\n" nil t (% (line-number-at-pos) helm-swoop-last-prefix-number))
     (goto-char (point-at-bol)))
   ;; For multiline highlight
   (save-excursion
     (goto-char (point-at-bol))
     (or (search-forward "\n" nil t helm-swoop-last-prefix-number)
         ;; For the end of buffer error
         (point-max)))))

(defun helm-swoop--target-word-overlay ($identity &optional $threshold)
  (interactive)
  (or $threshold (setq $threshold 2))
  (save-excursion
    (let (($pat (split-string helm-pattern " "))
          $o)
      (dolist ($wd $pat)
        ;; Each word must be 3 or more
        (when (< $threshold (length $wd))
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
            (overlay-put $o $identity t)))))))

;; helm action ------------------------------------------------

(defadvice helm-next-line (around helm-swoop-next-line disable)
  (let ((helm-move-to-line-cycle-in-source t))
    ad-do-it
    (when (called-interactively-p 'any)
      (helm-swoop--move-line-action))))

(defadvice helm-previous-line (around helm-swoop-previous-line disable)
  (let ((helm-move-to-line-cycle-in-source t))
    ad-do-it
    (when (called-interactively-p 'any)
      (helm-swoop--move-line-action))))

(defun helm-swoop--move-line-action ()
  (with-helm-window
    (let* (($key (helm-swoop--get-string-at-line))
           ($num (when (string-match "^[0-9]+" $key)
                   (string-to-number (match-string 0 $key)))))
      ;; Synchronizing line position
      (with-selected-window helm-swoop-synchronizing-window
        (progn
          (helm-swoop--goto-line $num)
          (with-current-buffer helm-swoop-target-buffer
            (delete-overlay helm-swoop-line-overlay)
            (helm-swoop--target-line-overlay-move))
          (recenter))))))

(defun helm-swoop--pattern-match ()
  "Overlay target words"
  (with-helm-window
    (when (< 2 (length helm-pattern))
      (with-selected-window helm-swoop-synchronizing-window
        (helm-swoop--delete-overlay 'target-buffer)
        (helm-swoop--target-word-overlay 'target-buffer)))))

;; core ------------------------------------------------

(defun helm-swoop--get-content (&optional $linum)
  "Get the whole content in buffer and add line number at the head.
If $linum is number, lines are separated by $linum"
  (let (($bufstr (if helm-swoop-speed-or-color
                     (buffer-substring (point-min) (point-max))
                   (buffer-substring-no-properties (point-min) (point-max))))
        $return)
    (with-temp-buffer
      (insert $bufstr)
      (goto-char (point-min))
      (let (($i 1))
        (insert (format "%s " $i))
        (while (search-forward "\n" nil t)
          (incf $i)
          (insert (format "%s " $i)))
        ;; Delete empty lines
        (unless $linum
          (goto-char (point-min))
          (while (re-search-forward "^[0-9]+\\s-*$" nil t)
            (replace-match ""))))
      (setq $return
            (if helm-swoop-speed-or-color
                (buffer-substring (point-min) (point-max))
              (buffer-substring-no-properties (point-min) (point-max)))))
    $return))

(defvar helm-swoop-map
  (let (($map (make-sparse-keymap)))
    (set-keymap-parent $map helm-map)
    (define-key $map (kbd "C-c C-e") 'helm-swoop-edit)
    (define-key $map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    (delq nil $map))
  "Keymap for helm-swoop")

(defun helm-c-source-swoop ()
  `((name . "Helm Swoop")
    (init . (lambda ()
              (unless helm-swoop-cache
                (with-current-buffer (helm-candidate-buffer 'local)
                  (insert ,(helm-swoop--get-content)))
                (setq helm-swoop-cache t))))
    (candidates-in-buffer)
    (get-line . ,(if helm-swoop-speed-or-color
                     'buffer-substring
                   'buffer-substring-no-properties))
    (keymap . ,helm-swoop-map)
    (header-line . "[C-c C-e] Edit mode, [M-i] apply all buffers")
    (action . (lambda ($line)
                (helm-swoop--goto-line
                 (when (string-match "^[0-9]+" $line)
                   (string-to-number (match-string 0 $line))))
                (when (re-search-forward
                       (mapconcat 'identity
                                  (split-string helm-pattern " ") "\\|")
                       nil t)
                  (goto-char (match-beginning 0)))
                (recenter)))
    (migemo) ;;? in exchange for those matches ^ $ [0-9] .*
    ))

(defun helm-c-source-swoop-multiline ($linum)
  `((name . "Helm Swoop Multiline")

    (candidates . ,(if helm-swoop-list-cache
                       (progn
                         (helm-swoop--split-lines-by
                          helm-swoop-list-cache "\n" $linum))
                     (helm-swoop--split-lines-by
                      (setq helm-swoop-list-cache
                            (helm-swoop--get-content t))
                      "\n" $linum)))
    (keymap . ,helm-swoop-map)
    (action . (lambda ($line)
                (helm-swoop--goto-line
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

(defun helm-swoop--scrolling-set (&optional $multiline)
  ;; Enable scrolling margin
  (if (boundp 'helm-swoop-last-prefix-number)
      (setq helm-swoop-last-prefix-number
            (or $multiline 1)) ;; $multiline is for resume
    (set (make-local-variable 'helm-swoop-last-prefix-number)
         (or $multiline 1)))
  ;; Modify scrolling temporarily
  (when helm-display-source-at-screen-top
    (setq helm-display-source-at-screen-top nil))
  (setq helm-completion-window-scroll-margin
          (+ 5 helm-swoop-last-prefix-number)))

(defun helm-swoop--scrolling-reset ()
  (setq helm-display-source-at-screen-top helm-swoop-at-screen-top)
  (setq helm-completion-window-scroll-margin helm-swoop-store-scroll-margin))

;; Delete cache when modified file is saved
(defun helm-swoop--clear-cache ()
  (if (boundp 'helm-swoop-cache) (setq helm-swoop-cache nil))
  (if (boundp 'helm-swoop-list-cache) (setq helm-swoop-list-cache nil)))
(add-hook 'after-save-hook 'helm-swoop--clear-cache)

(defun helm-swoop--restore ()
  (if (= 1 helm-exit-status) (helm-swoop-back-to-last-point t))
  (ad-disable-advice 'helm-next-line 'around 'helm-swoop-next-line)
  (ad-activate 'helm-next-line)
  (ad-disable-advice 'helm-previous-line 'around 'helm-swoop-previous-line)
  (ad-activate 'helm-previous-line)
  (remove-hook 'helm-update-hook 'helm-swoop--pattern-match)
  (setq helm-display-function helm-swoop-display-tmp)
  (setq helm-swoop-last-query helm-pattern)
  (helm-swoop--scrolling-reset)
  (mapc (lambda ($ov)
          (when (or (eq 'helm-swoop-target-line-face (overlay-get $ov 'face))
                    (eq 'helm-swoop-target-line-block-face
                        (overlay-get $ov 'face)))
            (delete-overlay $ov)))
        (overlays-in (point-min) (point-max)))
  (helm-swoop--delete-overlay 'target-buffer)
  (deactivate-mark t))

;;;###autoload
(defun helm-swoop (&optional $multiline $input)
  (interactive "p")
  "List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list."
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point (cons (point) (buffer-name (current-buffer))))
  (unless (boundp 'helm-swoop-last-query)
    (set (make-local-variable 'helm-swoop-last-query) ""))
  (setq helm-swoop-target-buffer (current-buffer))
  (helm-swoop--scrolling-set $multiline)
  ;; Overlay
  (setq helm-swoop-line-overlay (make-overlay (point) (point)))
  (overlay-put helm-swoop-line-overlay
               'face (if (< 1 helm-swoop-last-prefix-number)
                         'helm-swoop-target-line-block-face
                       'helm-swoop-target-line-face))
  ;; Cache
  (cond ((not (boundp 'helm-swoop-cache))
         (set (make-local-variable 'helm-swoop-cache) nil))
        ((buffer-modified-p)
         (setq helm-swoop-cache nil)))
  ;; Cache for multiline
  (cond ((not (boundp 'helm-swoop-list-cache))
         (set (make-local-variable 'helm-swoop-list-cache) nil))
        ((buffer-modified-p)
         (setq helm-swoop-list-cache nil)))
  (unwind-protect
      (progn
        ;; Modify window split function temporarily
        (setq helm-display-function helm-swoop-split-window-function)
        ;; For synchronizing line position
        (ad-enable-advice 'helm-next-line 'around 'helm-swoop-next-line)
        (ad-activate 'helm-next-line)
        (ad-enable-advice 'helm-previous-line 'around 'helm-swoop-previous-line)
        (ad-activate 'helm-previous-line)
        (ad-enable-advice 'helm-move--next-line-fn 'around
                          'helm-multi-swoop-next-line-cycle)
        (ad-activate 'helm-move--next-line-fn)
        (ad-enable-advice 'helm-move--previous-line-fn 'around
                          'helm-multi-swoop-previous-line-cycle)
        (ad-activate 'helm-move--previous-line-fn)
        (add-hook 'helm-update-hook 'helm-swoop--pattern-match)
        ;; Switch input
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
                   (setq $input $st))))
              ((helm-swoop--thing-at-point)
               (setq $input (helm-swoop--thing-at-point)))
              (t (setq $input "")))
        ;; First behavior
        (recenter)
        (move-beginning-of-line 1)
        (helm-swoop--target-line-overlay-move)
        ;; Execute helm
        (helm :sources
              (if (> helm-swoop-last-prefix-number 1)
                  (helm-c-source-swoop-multiline helm-swoop-last-prefix-number)
                (helm-c-source-swoop))
              :buffer helm-swoop-buffer
              :input $input
              :prompt helm-swoop-prompt
              :preselect
              ;; Get current line has content or else near one
              (if (string-match "^[\t\n\s]*$" (helm-swoop--get-string-at-line))
                  (save-excursion
                    (if (re-search-forward "[^\t\n\s]" nil t)
                        (format "^%s\s" (line-number-at-pos))
                      (re-search-backward "[^\t\n\s]" nil t)
                      (format "^%s\s" (line-number-at-pos))))
                (format "^%s\s" (line-number-at-pos)))
              :candidate-number-limit helm-swoop-candidate-number-limit))
    ;; Restore helm's hook and window function etc
    (helm-swoop--restore)))

;; Receive word from isearch ---------------
;;;###autoload
(defun helm-swoop-from-isearch ()
  "Invoke `helm-swoop' from isearch."
  (interactive)
  (let (($input (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (helm-swoop nil $input)))
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; For helm-resume ------------------------
(defadvice helm-resume-select-buffer
  (around helm-swoop-if-selected-as-resume activate)
  "Resume if *Helm Swoop* buffer selected as a resume
 when helm-resume with prefix"
  (if (boundp 'helm-swoop-last-query)
      ad-do-it
    ;; If the buffer hasn't called helm-swoop, just hide from options
    (let ((helm-buffers (delete helm-swoop-buffer helm-buffers)))
      ad-do-it))
  (when (and (equal ad-return-value helm-swoop-buffer)
             (boundp 'helm-swoop-last-query))
    (helm-swoop helm-swoop-last-prefix-number helm-swoop-last-query)
    (setq ad-return-value nil)))

(defadvice helm-resume (around helm-swoop-resume activate)
  "Resume if the last used helm buffer ishelm-swoop-buffer"
  (if (equal helm-last-buffer helm-swoop-buffer) ;; 1

      (if (boundp 'helm-swoop-last-query)  ;; 2
          (if (not (ad-get-arg 0)) ;; 3
              (helm-swoop helm-swoop-last-prefix-number helm-swoop-last-query))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers))) ad-do-it)) ;; 2 else
    ad-do-it) ;; 1 else
    )

;; For caret beginning-match -----------------------------
(defun helm-swoop--caret-match-delete ($o $aft $beg $end &optional $len)
  (if $aft
      (- $end $beg $len) ;; Unused argument? To avoid byte compile error
    (delete-region (overlay-start $o) (1- (overlay-end $o)))))

(defun helm-swoop-caret-match (&optional $resume)
  (interactive)
  (if (and (string-match "^Swoop\\:\s" ;; depend on helm-swoop-prompt
                         (buffer-substring-no-properties
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
          (overlay-put $o 'modification-hooks '(helm-swoop--caret-match-delete))
          (overlay-put $o 'display "^")
          (overlay-put $o 'evaporate t)))
    (if (minibufferp) (insert "^"))))

(unless (featurep 'helm-migemo)
  (define-key helm-map (kbd "^") 'helm-swoop-caret-match))

(provide 'helm-swoop)
;;; helm-swoop.el ends here
