;;; ~/.doom.d/config.el

(setq doom-theme 'doom-city-lights
      doom-localleader-key ","
      display-line-numbers-type 'relative
      line-number-mode nil
      initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; Graphic configurations
(when (display-graphic-p)
  (setq doom-font (font-spec :family "Hack" :size 14))

  (custom-set-faces '(cursor ((t (:background "#98f5ff")))))

  ;; Enable menu-bar-mode to fix focus issue
  (menu-bar-mode t)

  ;; Make titlebar match background color
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (mode-icons-mode))

;; truncate-lines in all buffers
(setq-default truncate-lines nil)
(setq-default global-visual-line-mode t)

;; Workaround for magithub authentication stuffs
(setq auth-sources '("~/.authinfo"))

;; Set command key as super on OSX
(setq mac-command-modifier 'super)

;; Set option key as meta on OSX
(setq mac-option-modifier 'meta)

(setq delete-by-moving-to-trash t)

(setq +workspaces-switch-project-function #'+counsel-fzf-find-project)

;; apib-mode
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

(with-eval-after-load "persp-mode-projectile-bridge-autoloads"
  (add-hook 'persp-mode-projectile-bridge-mode-hook
            #'(lambda ()
                (if persp-mode-projectile-bridge-mode
                    (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                  (persp-mode-projectile-bridge-kill-perspectives))))
  (add-hook 'after-init-hook
            #'(lambda ()
                (persp-mode-projectile-bridge-mode 1))
            t))

(global-evil-matchit-mode t)

(after! doom-modeline
  (remove-hook! 'doom-modeline-mode-hook #'column-number-mode)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-percent-position nil))

(after! osx-trash
  (when (eq system-type 'darwin)
    (osx-trash-setup)))

(after! dired
  (add-hook! dired-mode #'rspec-dired-mode)
  (map! :mode dired-mode
        :nv "." #'+dired-hydra/body
        [remap dired-find-file] #'dired-single-buffer
        [remap dired-up-directory] #'dired-single-up-directory))

;; Set default source and destination languages for Google Translate
(after! google-translate-core-ui
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi"))

;; Keybindings
(map! "s-c" #'evil-yank
      "s-v" #'yank
      "s-w" #'kill-current-buffer
      "s-W" #'delete-window
      "s-p" #'counsel-projectile-switch-project
      "s-o" #'+ivy/projectile-find-file
      "s-[" #'previous-buffer
      "s-]" #'next-buffer
      "s-1" #'+workspace-switch-to-0
      "s-2" #'+workspace-switch-to-1
      "s-3" #'+workspace-switch-to-2
      "s-4" #'+workspace-switch-to-3
      "s-5" #'+workspace-switch-to-4
      "s-6" #'+workspace-switch-to-5
      "s-7" #'+workspace-switch-to-6
      "s-8" #'+workspace-switch-to-7
      "s-9" #'+workspace-switch-to-8
      "s-0" #'+workspace-switch-to-9

      ;; Easier window navigation
      :nvi "C-h"  #'evil-window-left
      :nvi "C-j"  #'evil-window-down
      :nvi "C-k"  #'evil-window-up
      :nvi "C-l"  #'evil-window-right
      :nv "C-S-k" #'move-line-up
      :nv "C-S-j" #'move-line-down

      (:leader
        :nv "SPC" #'+counsel-fzf-find-project
        :nv "x" nil ;; Disable x prefix for scratch buffer

        (:prefix "o"
          :desc "List processes" :nv "x" #'list-processes
          :desc "Project sidebar" :nv "p" #'treemacs)

        (:prefix "b"
          :nv "s" nil
          :nv "k" nil
          :desc "Delete buffer" :nv "d"  #'kill-this-buffer)

        (:prefix "w"
          :nv "c" nil
          :nv "d"  #'evil-window-delete)

        (:prefix "/"
          :desc "Search this text in project" :nv "*" #'counsel-projectile-rg)

        (:prefix "p"
          :desc "Find dir" :nv "d" #'counsel-projectile-find-dir
          :desc "Find file in project" :nv "/" #'+counsel-fzf-find-project)

        (:prefix "g"
          :desc "Resolve conflicts" :n "r" #'smerge-hydra/body)

        (:prefix "w"
          :desc "evil-window-resize" :n "r" #'evil-window-resize-hydra/body)

        (:prefix "i"
          :desc "UUIDv4" :n "u" #'uuidgen
          (:prefix ("l" . "lorem-ipsum")
            :desc "list" :n "l" #'lorem-ipsum-insert-list
            :desc "sentences" :n "s" #'lorem-ipsum-insert-sentences
            :desc "paragraphs" :n "p" #'lorem-ipsum-insert-paragraphs)
          :desc "Emoji" :n "e" #'emoji-cheat-sheet-plus-insert)

        (:prefix "t"
          :desc "Truncate lines" :n "t" #'toggle-truncate-lines)

        (:prefix ("x" . "text-transform")
          :desc "Translate this text" :nv "g" #'google-translate-at-point
          (:prefix ("f" . "copy-as-format")
            :desc "Github" :nv "g" #'copy-as-format-github
            :desc "HTML" :nv "h" #'copy-as-format-html
            :desc "Markdown" :nv "m" #'copy-as-format-markdown
            :desc "Org" :nv "o" #'copy-as-format-org
            :desc "Slack" :nv "s" #'copy-as-format-slack))

        (:prefix "TAB"
          :desc "Display TAB bar" "TAB" #'+workspace-hydra/body)))

(unless (display-graphic-p)
  (require 'navigate)
  (map! :nvi "C-h" #'evil-tmux-navigate-left
        :nvi "C-j" #'evil-tmux-navigate-down
        :nvi "C-k" #'evil-tmux-navigate-up
        :nvi "C-l" #'evil-tmux-navigate-right))

(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

;; Modules
;; Evil
(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional _beg _end _type)
    (list (point-min) (point-max)))
  (evil-define-text-object evil-outer-defun (count &optional _beg _end _type)
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)))
  (add-to-list 'evil-motion-state-modes #'process-menu-mode)
  (map! :map evil-inner-text-objects-map "g" 'evil-inner-buffer
        :map evil-outer-text-objects-map "m" 'evil-outer-defun
        :map evil-motion-state-map "," nil))

;; Magit
(after! magit
  (set-popup-rule! "^magit:\s" :width 0.5 :side 'right :select t :modeline t :quit 'current)
  (setq magit-repository-directories '(("~/EH-Workspace" . 1)
                                       ("~/Workspace" . 1))
        magit-save-repository-buffers nil))

;; lang/org
(after! org
  (add-hook! 'org-pomodoro-finished-hook
    (terminal-notifier-notify "org-pomodoro" "Pomodoro finished!"))
  (map! "C-c c" #'org-capture
        "C-c n p" #'org-projectile-project-todo-completing-read
        :nv "C-c C-p" #'org-pomodoro)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (add-to-list 'org-modules 'org-drill)
  (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)
  (setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")
  (remove-hook! org-mode #'org-indent-mode)
  (setq org-startup-indented nil)
  (setq org-directory (expand-file-name "~/Workspace/orgs")
        org-agenda-files (list org-directory))
  ;; Org capture templates
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file "~/Workspace/orgs/inbox.org")
                                 "* ☛ TODO %i%?")))
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
  :when (featurep! :tools flycheck)
  :after apib-mode
  :config (add-hook! apib-mode #'flycheck-apib-setup))

;; js2-mode
(after! js2-mode
  (setq-default js-indent-level 2)
  (add-hook! js2-mode #'(add-node-modules-path prettier-js-mode))
  (map! :mode js2-mode
        (:leader
          (:prefix "p"
            :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))))

;; json-mode
(after! json-mode
  (setq-default js-indent-level 2))

;; projectile
(after! projectile
  ;; Configure npm project with projectile
  (projectile-register-project-type 'npm '("package.json")
                                    :test "yarn test"
                                    :test-suffix ".spec"
                                    :related-files-fn #'projectile-frontend-core-related-files))

;; Re-add visual-line-mode's fringes
(after! fringe-helper
  (add-to-list 'fringe-indicator-alist
               (list 'continuation
                     (fringe-lib-load fringe-lib-slash)
                     (fringe-lib-load fringe-lib-backslash))))

(after! rspec-mode
  (add-hook! rspec-compilation-mode #'inf-ruby-switch-from-compilation)
  (set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :width 0.3 :side 'right :quit 'current))

(after! treemacs
  (map! :mode treemacs-mode
        "C-h"  #'evil-window-left
        "C-j"  #'evil-window-down
        "C-k"  #'evil-window-up
        "C-l"  #'evil-window-right)
  (setq treemacs-show-cursor t)
  (treemacs-follow-mode t))

(after! helm
  (setq helm-ag-insert-at-point 'symbol))

(after! ivy
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)))

(after! enh-ruby-mode
  (map! :mode enh-ruby-mode
        (:leader
          (:prefix "p"
            :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))
        (:localleader
          (:prefix ("l" . "lsp")
            "f" #'lsp-format-buffer)
          (:prefix ("b" . "bundle"))
          (:prefix ("k" . "rake"))
          (:prefix ("r" . "robe"))
          (:prefix ("s" . "inf-ruby"))
          (:prefix ("t" . "rspec")))))

(after! all-the-icons
  (add-to-list 'all-the-icons-mode-icon-alist
               '(enh-ruby-mode all-the-icons-alltheicon "ruby-alt" :face all-the-icons-lred)))

(after! org-projectile
  (setq org-projectile-projects-file "~/Workspace/orgs/inbox.org")
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(after! reason-mode
  (add-hook! reason-mode #'lsp)
  (add-hook! reason-mode (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                    :major-modes '(ruby-mode enh-ruby-mode)
                    :priority 1
                    :multi-root t
                    :initialization-options  '(:diagnostics t)
                    :server-id 'enh-ruby-ls))
  (setq lsp-eldoc-render-all nil))

(after! company-lsp
  (setq company-lsp-cache-candidates 'auto))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-doc-enable nil))

(after! web-mode
  (add-hook! (html-mode css-mode sass-mode less-css-mode web-mode) #'lsp!))

(after! elisp-mode
  (map! :mode emacs-lisp-mode
        (:leader
          (:prefix "p"
            :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))))

(after! css-mode
  (setq css-indent-offset 2))

(after! forge
  (setq forge-topic-list-limit '(5 . 5)))

(after! hydra
  (setq hydra-hint-display-type 'message))

(after! prodigy
  (setq prodigy-view-truncate-by-default t)
  (setq prodigy-view-buffer-maximum-size 512)
  (set-evil-initial-state! 'prodigy-mode 'normal)
  (map! :mode prodigy-mode
        (:prefix "g"
          :nv "s" #'prodigy-start
          :nv "S" #'prodigy-stop)))

(after! tide
  (setq tide-hl-identifier-idle-time 2)
  (setq tide-sync-request-timeout 5))

(after! mode-icons
  (add-to-list 'mode-icons '("\\`JS2\\'" "js" xpm))
  (add-to-list 'mode-icons '("\\`Elisp\\'" "emacs" xpm))
  (add-to-list 'mode-icons '("\\`JSON\\'" #xe90b all-the-icons))
  (add-to-list 'mode-icons '("\\`BSDmakefile\\'" #xe679 file-icons))
  (add-to-list 'mode-icons '("\\`Gitignore\\'" #xf1d2 FontAwesome)))

(after! gist
  (set-evil-initial-state! 'gist-list-mode 'emacs))

(after! git-gutter
  (setq git-gutter:modified-sign "~"))

(after! doom-themes-ext-treemacs
  (require 'treemacs)
  (require 'icons-in-terminal)

  (treemacs-modify-theme "doom"
    :config
    (progn
      (let ((face-spec '(:inherit font-lock-doc-face :slant normal)))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "repo" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "repo"))
                              :extensions (root))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "file-text" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-faicon "file-text"))
                              :extensions (fallback "txt"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "folder-open" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-faicon "folder-open"))
                              :extensions (dir-open))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "folder" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-faicon "folder"))
                              :extensions (dir-closed))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "chevron-down" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "chevron-down"))
                              :extensions (tag-open))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "chevron-right" :v-adjust 0 :face face-spec))
                              :icon (format " %s " (icons-in-terminal-octicon "chevron-right"))
                              :extensions (tag-closed))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "chevron-down" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "chevron-down"))
                              :extensions (tag-node-open))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "chevron-right" :v-adjust 0 :face face-spec))
                              :icon (format " %s " (icons-in-terminal-octicon "chevron-right"))
                              :extensions (tag-node-closed))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "tag" :v-adjust 0 :face face-spec))
                              :icon (format " %s " (icons-in-terminal-faicon "tag"))
                              :extensions (tag-leaf))

        ;; Programming Languages
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "elisp" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "elisp"))
                              :extensions ("el" "elc"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "javascript" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "javascript"))
                              :extensions ("js"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jsx2-alt" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "jsx"))
                              :extensions ("jsx"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "typescript" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "typescript"))
                              :extensions ("ts"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "reason" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "reason"))
                              :extensions ("re" "rei"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "ruby-alt" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "ruby-alt"))
                              :extensions ("rb"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "python" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "python"))
                              :extensions ("py"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "rust" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "rust"))
                              :extensions ("rs"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "database" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "database"))
                              :extensions ("sql"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "terminal" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "terminal"))
                              :extensions ("sh" "zsh" "fish"))

        ;; Mark-up languages
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "css3" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-faicon "css3"))
                              :extensions ("css"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "html5" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "html5"))
                              :extensions ("html" "htm"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "code" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "code"))
                              :extensions ("erb" "haml"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "org" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "org"))
                              :extensions ("org"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "tex" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "tex"))
                              :extensions ("tex"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "markdown" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "markdown"))
                              :extensions ("md" "markdown"))

        ;; Project configuration
        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "git-square" :v-adjust -0.1 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "git"))
                              :extensions ("gitignore" "git" "gitconfig" "gitmodules"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "gnu" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "gnu"))
                              :extensions ("makefile"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "config" :v-adjust -0.1 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "config"))
                              :extensions ("conf" "cfg" "yaml" "yml" "json" "xml" "toml" "cson" "ini" "zshrc" "vimrc"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "dockerfile" :v-adjust -0.1 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "dockerfile"))
                              :extensions ("dockerfile"))

        ;; Media file types
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "word" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-fileicon "word"))
                              :extensions ("docx"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "file-pdf" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "file-pdf"))
                              :extensions ("pdf"))

        (treemacs-create-icon :icon (format " %s " (icons-in-terminal-octicon "file-zip" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "file-zip"))
                              :extensions ("zip" "xz" "tar" "gz" "7z" "rar"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "file-media" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-octicon "file-media"))
                              :extensions ("jpg" "jpeg" "png" "gif" "ico" "tif" "tiff"
                                           "svg" "bmp" "psd" "ai" "eps" "indd"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-material "movie" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-material "movie"))
                              :extensions ("mov" "avi" "mp4" "webm" "webp" "mkv"))

        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "music" :v-adjust 0 :face face-spec))
                              :fallback (format " %s " (icons-in-terminal-faicon "music"))
                              :extensions ("wav" "mp3" "ogg" "midi"))))))

;;;;;;;;;; Functions ;;;;;;;;;;
(defhydra smerge-hydra (:hint nil)
  ("n" smerge-next "next hunk" :column "Movement")
  ("N" smerge-prev "prev hunk")
  ("k" evil-previous-line "move up")
  ("j" evil-next-line "move down")
  ("C-u" evil-scroll-up "scroll up")
  ("C-d" evil-scroll-down "scroll down")
  ("b" smerge-keep-base "keep base" :column "Merge action")
  ("m" smerge-keep-mine "keep mine")
  ("a" smerge-keep-all "keep all")
  ("o" smerge-keep-other "keep other")
  ("c" smerge-keep-current "keep current")
  ("C" smerge-combine-with-next "combine with next")
  ("u" undo-tree-undo "undo" :column "Other")
  ("r" smerge-refine "refine")
  ("q" nil "quit"))

(defhydra evil-window-resize-hydra (:hint nil)
  ("k" (evil-window-increase-height 10) "increase height by 10 rows")
  ("j" (evil-window-decrease-height 10) "decrease height by 10 rows")
  ("l" (evil-window-increase-width 10) "increase width by 10 columns")
  ("h" (evil-window-decrease-width 10) "decrease width by 10 columns")
  ("q" nil "quit"))

(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 (executable-find "terminal-notifier")
                 "-title" title
                 "-message" message
                 "-sound" "default"
                 "-activate" "org.gnu.Emacs"
                 "-group" "org.gnu.Emacs"
                 "-appIcon" "/Users/qhuyduong/Workspace/inventory/org-unicorn.png"))

(defun projectile-frontend-core-related-files (path)
  (when (string-match "\\(.*\\)\/\\(.*\\)$" path)
    (let* ((dir (match-string 1 path))
           (file-name (match-string 2 path))
           (base-file-name (car (split-string file-name "\\."))))
      (if (projectile-test-file-p file-name)
          (list :impl (concat dir "/../" base-file-name ".js"))
        (list :test (concat dir "/__tests__/" base-file-name ".spec.js"))))))

(defhydra +dired-hydra (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(dolist (i (number-sequence 0 9))
  (eval `(defun ,(intern (format "+workspace-switch-to-%s" i)) nil
           ,(format "Switch to workspace %s" i)
           (interactive)
           (+workspace/switch-to ,i))))

(defhydra +workspace-hydra (:hint nil)
  "
%s(+workspace/display)"
  ("1" +workspace-switch-to-0)
  ("2" +workspace-switch-to-1)
  ("3" +workspace-switch-to-2)
  ("4" +workspace-switch-to-3)
  ("5" +workspace-switch-to-4)
  ("6" +workspace-switch-to-5)
  ("7" +workspace-switch-to-6)
  ("8" +workspace-switch-to-7)
  ("9" +workspace-switch-to-8)
  ("0" +workspace-switch-to-9))

(defun +counsel-fzf-find-project (&optional dir)
  (interactive)
  (let ((project-dir (if dir dir (projectile-project-root))))
    (counsel-fzf nil dir (format "[%s] " (file-name-nondirectory (directory-file-name project-dir))))))

(defun evil-tmux-navigate-left ()
  "Navigate to the left tmux pane"
  (interactive)
  (tmux-navigate "left"))

(defun evil-tmux-navigate-down ()
  "Navigate to the below tmux pane"
  (interactive)
  (tmux-navigate "down"))

(defun evil-tmux-navigate-up ()
  "Navigate to the above tmux pane"
  (interactive)
  (tmux-navigate "up"))

(defun evil-tmux-navigate-right ()
  "Navigate to the right tmux pane"
  (interactive)
  (tmux-navigate "right"))
