List the multi lines to another buffer, which is able to squeeze by any words you input. At the same time, the original buffer's cursor is jumping line to line according to moving up and down the line list.

### Youtube video

I don't take your time too much. Only 30 seconds. 

[here](http://www.youtube.com/watch?v=RfasCCuCEgM)

### Feature

* Squeeze the all lines list by your input
* Highlight multiple matched pattern
* Jumping line to line according to list buffer's move
* Cache result
* Go back to the last line

Even if 10,000 lines buffer such as non-compression jQuery,
First time it takes 0 ~ 1.0s though, but from the second time,
it will be almost no time by employing cache list.
Cache is valid until you modify the buffer.

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

`M-x helm-swoop` or just press the keybind you set. 
It's able to use words within region as search query when it called.

### Require

[helm.el](https://github.com/emacs-helm/helm)

