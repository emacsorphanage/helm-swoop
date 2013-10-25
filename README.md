List the multi lines to another buffer, which is able to squeeze by any words you input. At the same time, the original buffer's cursor is jumping line to line according to moving up and down the line list.

## Youtube video

I do not take your time too much. This is only 30 seconds. 

[here](http://www.youtube.com/watch?v=RfasCCuCEgM)

### Config

```elisp
;; helm from https://github.com/emacs-helm/helm
(require 'helm-config)
(helm-mode 1)

;; Locate the helm-swwop folder to your path
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

;; Change target line color face
(setq helm-swoop-target-line-face
  '((foreground-color . "#333333")
    (background-color . "#eeee00")
    (font-weight . "bold")
    ;;(underline . t)
    ))

;; Change target word color face
(setq helm-swoop-target-word-face
  '((foreground-color . "#ffffff")
    (background-color . "#7700ff")))
```

### Usage

`M-x helm-swoop` or just press the keybind you set (above [M-i]). 
It can use words within region as search query when it called.

### Require

[helm.el](https://github.com/emacs-helm/helm)

