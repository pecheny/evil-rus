# evil-rus

 Evil operators ignore your input method and treat argument as if it was typed in default english layout.
 This package aimed to fix this issue. Though it may have limitations.
 Original author khaoos-abominable had done the solution in personal config.
 https://github.com/khaoos-abominable/dotfiles/blob/master/spacemacs/dotspacemacs
 This version is extracted for use as a package especcially for doom users. Tested with doom and russian-computer input-method.

## Installation

 Add to `packages.el`:

 (package! projectile
   :recipe (:host github :repo "pecheny/evil-rus" ))

 and to `config.el`:
```emacs-lisp
 (define-key evil-normal-state-map (kbd "r") 'evil-rus-evil-replace)
 (define-key evil-motion-state-map (kbd "f") 'evil-rus-evil-find-char)
 (define-key evil-motion-state-map (kbd "t") 'evil-rus-evil-find-char-to)
 (define-key evil-motion-state-map (kbd "F") 'evil-rus-evil-find-char-backward)
 (define-key evil-motion-state-map (kbd "T") 'evil-rus-evil-find-char-to-backward)
```
 this commands has no default bindings so pick ones you like:
```emacs-lisp
 (map! :vn "gss" 'evil-rus-avy-goto-char-2)
 (map! :vn "gsd" 'evil-rus-avy-goto-word-or-subword-1)
 (map! :vn "gsx" 'evil-rus-avy-goto-char)
 (define-key evil-normal-state-map (kbd "M-i") 'evil-rus-insert-one-char)
 (define-key evil-normal-state-map (kbd "M-a") 'evil-rus-append-one-char)
```
 See more: https://github.com/emacs-evil/evil/issues/605
