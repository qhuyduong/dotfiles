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

;; rubocopfmt hook
(with-eval-after-load 'enh-ruby-mode
  (add-hook 'enh-ruby-mode-hook #'rubocopfmt-mode))

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'rubocopfmt-mode))

;; Icons in dired
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;; Workaround for all-the-icons-dired
(require 'font-lock+)

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

;; indent-guide
(indent-guide-global-mode)

;; Magit
(setq magit-repository-directories '(("~/EH-Workspace" . 0)
                                     ("~/Workspace" . 0))
      magit-save-repository-buffers nil)

;; lang/org
(setq org-directory (expand-file-name "~/orgs")
      org-agenda-files (list org-directory))
