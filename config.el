;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Emacs GUI settings
(setq doom-font (font-spec :family "Iosevka" :size 16))
(setq doom-theme 'doom-city-lights)
(setq doom-localleader-key ",")
(setq show-trailing-whitespace t)
(setq display-line-numbers-type 'relative)

;; Use fancy icons for neotree
(setq doom-neotree-file-icons t)

;; truncate-lines in all buffers
(setq-default truncate-lines nil)
(setq-default global-visual-line-mode t)

;; Make titlebar match background color
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Make Emacs fullscreen by default
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Set command key as super on OSX
(setq mac-command-modifier 'super)

;; Set option key as meta on OSX
(setq mac-option-modifier 'meta)

;; Reduce which-key delay
(setq which-key-idle-delay 0.5)

;; Workaround for magithub authentication stuffs
(setq auth-sources '("~/.authinfo"))

(when (eq system-type 'darwin)
  (osx-trash-setup))
(setq delete-by-moving-to-trash t)

;; Icons in dired
(after! dired
  (require 'font-lock+)
  ;; Suppress warning with GNU ls in Dired
  (setq dired-use-ls-dired nil)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  (diredp-toggle-find-file-reuse-dir t)
  ;; Make diredp less colorful
  (dolist (face '(diredp-dir-priv
                  diredp-read-priv
                  diredp-write-priv
                  diredp-exec-priv
                  diredp-file-suffix
                  diredp-link-priv
                  diredp-rare-priv
                  diredp-number
                  diredp-date-time))
    (set-face-foreground face (face-foreground 'default)))
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil))

;; Set default source and destination languages for Google Translate
(after! google-translate-core-ui
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi"))

;; Keybindings
(map! "s-c"  #'evil-yank
      "s-v"  #'yank
      (:leader
        :nv "x" nil
        (:prefix "/"
          :desc "Search this text in project" :nv "*"  #'counsel-rg-thing-at-point)
        (:prefix "p"
          :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test
          :desc "Regenerate tags" :n "G" #'projectile-regenerate-tags)
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
          :desc "Emoji" :n "e" #'emoji-cheat-sheet-plus-insert)
        (:prefix "t"
          :desc "Truncate lines" :n "t" #'toggle-truncate-lines)
        (:desc "text-transform" :prefix "x"
          :desc "Translate this text" :nv "g" #'google-translate-at-point
          (:desc "copy-as-format" :prefix "f"
            :desc "Github" :nv "g" #'copy-as-format-github
            :desc "HTML" :nv "h" #'copy-as-format-html
            :desc "Markdown" :nv "m" #'copy-as-format-markdown
            :desc "Org" :nv "o" #'copy-as-format-org
            :desc "Slack" :nv "s" #'copy-as-format-slack)))
      (:localleader
        :prefix "d"
        :nv "g" #'dumb-jump-go
        :nv "G" #'dumb-jump-go-other-window))

;; Emacs sometimes registers C-s-f as this weird keycode
(global-set-key (kbd "<C-s-268632070>") #'toggle-frame-fullscreen)

;; Modules
;; Evil
(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (map! :map evil-inner-text-objects-map "g" 'evil-inner-buffer
        :map evil-motion-state-map "," nil))

;; indent-guide
(indent-guide-global-mode)

;; Magit
(after! magit
  (setq magit-repository-directories '(("~/EH-Workspace" . 1)
                                       ("~/Workspace" . 1))
        magit-save-repository-buffers nil))

;; lang/org
(after! org
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (add-to-list 'org-modules 'org-drill)
  (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)
  (setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")
  (remove-hook 'org-mode-hook #'org-indent-mode)
  (setq org-startup-indented nil)
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

;; flycheck-apib
(def-package! flycheck-apib
  :when (featurep! :feature syntax-checker)
  :after apib-mode
  :config (add-hook 'apib-mode-hook #'flycheck-apib-setup))

;; apib-mode
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; js2-mode
(after! js2-mode
  (setq-default js-indent-level 2)
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'js2-mode-hook #'run-import-js)
  (add-hook 'js2-mode-hook #'(lambda ()
                               (add-hook 'before-save-hook #'import-js-fix nil t)))
  (add-hook 'js2-mode-hook #'prettier-js-mode))

;; json-mode
(after! json-mode
  (setq-default js-indent-level 2))

;; projectile
(after! projectile
  ;; Configure npm project with projectile
  (projectile-register-project-type 'npm '("package.json")
                                    :test "npm run test"
                                    :test-suffix ".spec"))

;; Re-add visual-line-mode's fringes
(after! fringe-helper
  (add-to-list 'fringe-indicator-alist
               (list 'continuation
                     (fringe-lib-load fringe-lib-slash)
                     (fringe-lib-load fringe-lib-backslash))))

;; Rspec doesn't use RVM!
(after! rspec-mode
  (setq rspec-use-rvm nil))

(after! flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(after! treemacs
  (setq doom-treemacs-use-generic-icons nil))

(after! ivy
  (assq-delete-all 'counsel-ag ivy-display-functions-alist))

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
  ("k" (evil-window-increase-height 10) "increase height by 10 rows")
  ("j" (evil-window-decrease-height 10) "decrease height by 10 rows")
  ("l" (evil-window-increase-width 10) "increase width by 10 columns")
  ("h" (evil-window-decrease-width 10) "decrease width by 10 columns")
  ("q" nil "quit"))
