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

;; Workaround for magithub authentication stuffs
(setq auth-sources '("~/.authinfo"))

(when (eq system-type 'darwin)
  (osx-trash-setup))
(setq delete-by-moving-to-trash t)

;; rubocopfmt hook
(add-hook! enh-ruby-mode #'rubocopfmt-mode)
(add-hook! ruby-mode #'rubocopfmt-mode)

;; Icons in dired
(after! dired
  (require 'font-lock+)
  ;; Suppress warning with GNU ls in Dired
  (setq dired-use-ls-dired nil)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Set default source and destination languages for Google Translate
(after! google-translate-core-ui
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi"))

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
(after! magit
  (setq magit-repository-directories '(("~/EH-Workspace" . 0)
                                       ("~/Workspace" . 0))
        magit-save-repository-buffers nil))

;; lang/org
(after! org
  (setq org-directory (expand-file-name "~/orgs")
        org-agenda-files (list org-directory))
  ;; Org capture templates
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/orgs/inbox.org" "Tasks")
                                 "* ☛ TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/orgs/tickler.org" "Tickler")
                                 "* %i%? \n %U")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with helm/ivy
  (setq org-outline-path-complete-in-steps nil)
  ;; Org TODO keywords
  (setq org-todo-keywords '((sequence "☛ TODO" "⚑ IN-PROGRESS" "|" "✓ DONE" "✘ CANCELED")))
  (setq org-todo-keyword-faces '(("☛ TODO" . (:foreground "grey"))
                                 ("⚑ IN-PROGRESS" . (:foreground "yellow"))
                                 ("✓ DONE" . (:foreground "green"))
                                 ("✘ CANCELED" . (:foreground "red")))))
