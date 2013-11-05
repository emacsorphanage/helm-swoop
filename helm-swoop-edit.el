
(eval-when-compile (require 'cl))
(require 'helm)

(defun helm-swoop-clear--edit-buffer ()
  (let ((inhibit-read-only t))
    (dolist ($ov (overlays-in (point-min) (point-max)))
      (when (overlay-get $ov 'helm-swoop-edit)
        (delete-overlay $ov)))
    (set-text-properties (point-min) (point-max) nil)
    (goto-char (point-min))
    (erase-buffer)))

(defun helm-swoop-collect--edited-lines ()
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
  (interactive)
  (with-current-buffer (get-buffer-create "*Helm Swoop Edit*")
    (helm-swoop-clear--edit-buffer)
    (let ($bufstr)
      ;; Get target line number for edit
      (with-current-buffer "*Helm Swoop*"
        (setq $bufstr (buffer-substring-no-properties
                       (point-min) (point-max))))

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
                       (propertize " [C-x C-s] Complete, [C-c C-g] cancel"
                                   'face 'helm-bookmark-addressbook)))
        ;; Line number and editable area
        (while (re-search-forward "^\\([0-9]+\s\\)\\(.*\\)$" nil t)
          (let (($bol1 (match-beginning 1))
                ($eol1 (match-end 1))
                ($bol2 (match-beginning 2))
                ($eol2 (match-end 2)))
            ;; Line number
            (add-text-properties $bol1 $eol1
                                 '(face font-lock-function-name-face
                                   intangible t))
            ;; Editable area
            (remove-text-properties $bol2 $eol2 '(read-only t))
            ;;(add-text-properties $bol2 $eol2 '(face helm-match))
            ;; For line tail
            (set-text-properties $eol2 (or (1+ $eol2) (point-max))
                                 '(read-only t rear-nonsticky t))
            )))))
  (other-window 1)
  (switch-to-buffer "*Helm Swoop Edit*")
  (goto-char (point-min))
  (if (string-match "^[0-9]+" $candidate)
      (re-search-forward
       (concat "^" (match-string 0 $candidate)) nil t))
  (local-set-key (kbd "C-x C-s") 'helm-swoop--edit-complete)
  (local-set-key (kbd "C-c C-g") 'helm-swoop--edit-cancel))

(defun helm-swoop--edit-complete ()
  "Apply changes and kill temporary edit buffer"
  (interactive)
  (let (($list (helm-swoop-collect--edited-lines)))
    (with-current-buffer helm-swoop-target-buffer
      ;; Replace from the end of buffer
      (save-excursion
      (loop for ($k . $v) in $list
            do (progn
                 (goto-char (point-min))
                 (delete-region (point-at-bol $k) (point-at-eol $k))
                 (goto-char (point-at-bol $k))
                 (insert $v)))))
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer "*Helm Swoop Edit*")))
  ;; Remain overlay problem ?
  (helm-swoop-delete-overlay)
  (message "Successfully helm-swoop-edit applied to original buffer"))

(defun helm-swoop--edit-cancel ()
  "Cancel edit and kill temporary buffer"
  (interactive)
  (select-window helm-swoop-synchronizing-window)
  (kill-buffer (get-buffer "*Helm Swoop Edit*"))
  ;; Remain overlay problem ?
  (helm-swoop-delete-overlay)
  (message "helm-swoop-edit canceled"))

(defun helm-swoop-edit ()
  (interactive)
  (helm-quit-and-execute-action 'helm-swoop--edit))

(provide 'helm-swoop-edit)
;;; helm-swoop.el ends here
