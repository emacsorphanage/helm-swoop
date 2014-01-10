List the multi lines to another buffer, which is able to squeeze by any words you input. At the same time, the original buffer's cursor is jumping line to line according to moving up and down the line list.

![helm-swoop](https://github.com/ShingoFukuyama/helm-swoop/raw/master/image/helm-swoop.gif)

### Feature

* Culling all lines in buffer with your input
* Highlight multiple matched pattern
* Jumping line to line according to list buffer's move
* Cache result until modifies the buffer
* Go back to the last line
* Multi separated line culling
* Culling lines are editable

### Usage

#### Now helm-swoop has several ways.

* `M-x helm-swoop` when region active
* `M-x helm-swoop` when the cursor is at any symbol
* `M-x helm-swoop` when the cursor is not at any symbol
* `M-3 M-x helm-swoop` or `C-u 5 M-x helm-swoop` multi separated line culling
* `M-x helm-multi-swoop` multi-occur like feature
* `M-x helm-multi-swoop-all` apply all buffers
* `C-u M-x helm-multi-swoop` apply last selected buffers from the second time
* During isearch `M-i` to hand the word over to helm-swoop
* During helm-swoop `M-i` to hand the word over to helm-multi-swoop-all
* While doing `helm-swoop` press `C-c C-e` to edit mode, apply changes to original buffer by `C-x C-s`

It's able to use words within a region or a word at symbol as search query when it called. Also use a keybind you set just type like M-i instead of `M-x helm-swoop`. 

#### Edit mode
While doing `helm-swoop` type `C-c C-e` to enter the edit mode.
Before enter the edit mode, you can choose some lines marked by `C-SPC` or `M-SPC` to edit.
Apply changes to original buffer type `C-x C-s`.

#### Across multiple buffers

##### `M-x helm-multi-swoop`
1. Select any buffers by [C-SPC] or [M-SPC]
2. Press [RET] to start helm-multi-swoop

![helm-multi-swoop](https://github.com/ShingoFukuyama/helm-swoop/raw/master/image/helm-multi-swoop.gif)

##### `M-x helm-multi-swoop-all`
Skip select phase and apply all buffers.

##### `C-u M-x helm-multi-swoop`
Skip select phase and apply last selected buffers, if you have done helm-multi-swoop before.


#### Multiline behavior 
`M-4 M-x helm-swoop` or `C-u 4 M-x helm-swoop`

![helm-swoop2](https://github.com/ShingoFukuyama/helm-swoop/raw/master/image/helm-swoop2.gif)

### Config

```elisp
;; helm from https://github.com/emacs-helm/helm
(require 'helm)

;; Locate the helm-swoop folder to your path
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
```

### Require

[helm.el](https://github.com/emacs-helm/helm)
