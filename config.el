;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Emacs GUI settings
(setq doom-font (font-spec :family "Monaco" :size 14))
(setq doom-theme 'doom-city-lights)
(setq doom-localleader-key ",")
(setq show-trailing-whitespace t)

;; Make titlebar match background color
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Set option key as meta on OSX
(setq mac-option-modifier 'meta)

;; Reduce which-key delay
(setq which-key-idle-delay 0.5)

;; Workaround for magithub authentication stuffs
(setq auth-sources '("~/.authinfo"))

;; Rspec doesn't use RVM!
(setq rspec-use-rvm nil)

(when (eq system-type 'darwin)
  (osx-trash-setup))
(setq delete-by-moving-to-trash t)

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
      :n "C-l"   #'evil-window-right
      (:leader
        (:prefix "/"
          :desc "Search this text in project" :nv "*"  #'counsel-rg-thing-at-point)
        (:prefix "p"
          :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test)
        (:prefix "g"
          :desc "Resolve conflicts" :n "r" #'hydra-smerge/body)
        (:prefix "w"
          :desc "evil-window-resize" :n "r" #'hydra-evil-window-resize/body)
        (:prefix "i"
          :desc "UUIDv4" :n "u" #'uuidgen
          (:desc "lorem-ipsum" :prefix "l"
            :desc "list" :n "l" #'lorem-ipsum-insert-list
            :desc "sentences" :n "s" #'lorem-ipsum-insert-sentences
            :desc "paragraphs" :n "p" #'lorem-ipsum-insert-paragraphs)
          :desc "Emoji" :n "e" #'emoji-cheat-sheet-plus-insert)))

;; Modules
;; Evil
(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (map! :map evil-inner-text-objects-map "g" 'evil-inner-buffer
        :map evil-motion-state-map "," nil))

;; indent-guide
(indent-guide-global-mode)

;; flycheck-pos-tip
(flycheck-pos-tip-mode)

;; Magit
(after! magit
  (setq magit-repository-directories '(("~/EH-Workspace" . 1)
                                       ("~/Workspace" . 1))
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

;;;;;;;;;; Chores ;;;;;;;;;;

(defun counsel-rg-thing-at-point ()
  (interactive)
  (counsel-rg (ivy-thing-at-point)))

(defhydra hydra-smerge (:hint nil)
  "
Movement^^^^            Merge action^^           Other
------------------^^^^  ---------------------^^  -----------
[_n_]^^    next hunk    [_b_] keep base          [_u_] undo
[_N_/_p_]  prev hunk    [_m_] keep mine          [_r_] refine
[_k_]^^    move up      [_a_] keep all           [_q_] quit
[_j_]^^    move down    [_o_] keep other
[_C-u_]^^  scroll up    [_c_] keep current
[_C-d_]^^  scroll down  [_C_] combine with next"
  ("n" smerge-next)
  ("N" smerge-prev)
  ("p" smerge-prev)
  ("j" evil-next-line)
  ("k" evil-previous-line)
  ("C-u" evil-scroll-up)
  ("C-d" evil-scroll-down)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("a" smerge-keep-all)
  ("o" smerge-keep-other)
  ("c" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("u" undo-tree-undo)
  ("r" smerge-refine)
  ("q" nil))

(defhydra hydra-evil-window-resize (:hint nil)
  ("k" evil-window-increase-height "increase height")
  ("j" evil-window-decrease-height "decrease height")
  ("l" evil-window-increase-width "increase width")
  ("h" evil-window-decrease-width "decrease width")
  ("q" nil "quit"))
