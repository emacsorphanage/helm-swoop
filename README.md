List the whole line to another buffer, which is able to squeeze by any words you input. At the same time, the original buffer's cursor is jumping line to line according to moving up and down the line list.

### Config

```elisp
(add-to-list 'load-path "/path/to/helm-swoop")
(require 'helm-swoop)

;; Change the keybinds to whatever you like
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

;; Change target line color face
(setq helm-swoop-target-line-face
  '((foreground-color . "#333333")
    (background-color . "#eeee00")
    (font-weight . "bold")
    ;;(underline . t)
    ))
```

### Require

[helm.el](https://github.com/emacs-helm/helm)
