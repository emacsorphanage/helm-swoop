List the multi lines to another buffer, which is able to squeeze by any words you input. At the same time, the original buffer's cursor is jumping line to line according to moving up and down the line list.

### Youtube

I don't take your time too much. Only 28 seconds. 

[Youtube video here](http://www.youtube.com/embed/Mo5OSQUM51g?rel=0)

### Feature

* Squeeze all lines in buffer with your input
* Highlight multiple matched pattern
* Jumping line to line according to list buffer's move
* Cache result
* Go back to the last line

Even if 10,000 lines buffer such as non-compression jQuery.
First time it takes 0 ~ 1.0s though, but from the second time,
it will be almost no time by employing cache list.
Cache is valid until you modify the buffer.


### Usage

Now this has three way.

* `M-x helm-swoop` when region active
* `M-x helm-swoop` when the cursor is at any symbol
* `M-x helm-swoop` when the cursor is not at any symbol

It's able to use words within region or a word at symbol as search query when it called. Also use keybind you set just type like M-i instead of `M-x helm-swoop`.

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

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
```

### Require

[helm.el](https://github.com/emacs-helm/helm)

#### Optional Require

For Japanese input system.

[helm-migemo.el](https://github.com/emacs-helm/helm-migemo)

