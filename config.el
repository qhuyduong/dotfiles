;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Emacs GUI settings
(setq doom-font (font-spec :family "Monaco" :size 14))
(setq doom-theme 'doom-city-lights)
;; Make titlebar match background color
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Reduce which-key delay
(setq which-key-idle-delay 0.5)

;; Keybindings
;; Easier window navigation
(map! :n "C-h"   #'evil-window-left
      :n "C-j"   #'evil-window-down
      :n "C-k"   #'evil-window-up
      :n "C-l"   #'evil-window-right)

;; Modules
;; Evil
(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

