;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Global settings
(setq doom-theme 'doom-palenight
      doom-localleader-key ","
      display-line-numbers-type 'relative
      initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
      doom-font (font-spec :family "Fira Code" :size 18)
      default-input-method "vietnamese-telex"
      vc-handled-backends '(Git))

(custom-set-faces '(cursor ((t (:background "#98f5ff")))))

;; Make titlebar match background color
(when IS-MAC
  ;; Set command key as super on OSX
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; truncate-lines in all buffers
(setq-default truncate-lines nil
              global-visual-line-mode t)

;; Workaround for magithub authentication stuffs
(add-to-list 'auth-sources "~/.authinfo")

(setq delete-by-moving-to-trash t)

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

;; Global modes
;; Enable menu-bar-mode to fix focus issue on macOS
(when IS-MAC
  (menu-bar-mode t))
(global-evil-matchit-mode t)

(after! doom-modeline
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-major-mode-icon t))

(when IS-MAC
  (after! osx-trash
    (osx-trash-setup)))

(after! dired
  (add-hook! dired-mode #'rspec-dired-mode)
  (map! :mode dired-mode
        :nv "." #'+dired-hydra/body
        [remap dired-find-file] #'dired-single-buffer
        [remap dired-up-directory] #'dired-single-up-directory))

;; Set default source and destination languages for Google Translate
(after! google-translate-core-ui
  (setq google-translate-backend-method 'curl)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi"))

;; Keybindings
(map! ;; Easier window navigation
      :nvi "C-h"  #'evil-window-left
      :nvi "C-j"  #'evil-window-down
      :nvi "C-k"  #'evil-window-up
      :nvi "C-l"  #'evil-window-right
      :nv "C-S-k" #'move-line-up
      :nv "C-S-j" #'move-line-down

      (:leader
        :nv "x" nil ;; Disable x prefix for scratch buffer

        (:prefix "o"
          :desc "List processes" :nv "x" #'list-processes)

        (:prefix "p"
          :desc "Find dir" :nv "d" #'counsel-projectile-find-dir)

        (:prefix "i"
          :desc "UUIDv4" :n "u" #'insert-random-uuid
          (:prefix ("l" . "lorem-ipsum")
            :desc "list" :n "l" #'lorem-ipsum-insert-list
            :desc "sentences" :n "s" #'lorem-ipsum-insert-sentences
            :desc "paragraphs" :n "p" #'lorem-ipsum-insert-paragraphs)
          :desc "Emoji" :n "e" #'emoji-cheat-sheet-plus-insert)

        (:prefix ("x" . "text-transform")
          :desc "Translate this text" :nv "g" #'google-translate-at-point
          (:prefix ("f" . "copy-as-format")
            :desc "Github" :nv "g" #'copy-as-format-github
            :desc "HTML" :nv "h" #'copy-as-format-html
            :desc "Markdown" :nv "m" #'copy-as-format-markdown
            :desc "Org" :nv "o" #'copy-as-format-org
            :desc "Slack" :nv "s" #'copy-as-format-slack))))

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
  (setq magit-repository-directories '(("~/workspace" . 1))
        +magit-default-clone-url "git@github.com:%s/%s"))

;; lang/org
(after! org
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)
  (setq org-startup-indented nil)
  (setq org-directory (expand-file-name "~/workspace/orgs")
        org-agenda-files (list org-directory))
  ;; Org capture templates
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file "~/workspace/orgs/inbox.org")
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

;; apib-mode
(use-package! apib-mode
  :mode "\\.apib\\'")

;; flycheck-apib
(use-package! flycheck-apib
  :when (featurep! :checkers syntax)
  :after apib-mode
  :config (add-hook! apib-mode #'flycheck-apib-setup))

;; rjsx-mode
(use-package! rjsx-mode
  :mode "\\.js*\\'"
  :config
  (setq-default js-indent-level 2)
  (add-hook! rjsx-mode #'prettier-js-mode)
  (add-hook! rjsx-mode (add-hook '+lookup-file-functions #'find-relative-file-or-folder nil t))
  (set-company-backend! 'rjsx-mode '(company-tide company-files :with company-yasnippet))
  (map! :mode rjsx-mode
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
                                    :related-files-fn #'projectile-frontend-core-related-files)
  ;; Workaround for Rails 6
  (projectile-register-project-type 'rails-rspec '("Gemfile")
                                    :test "bundle exec rspec"
                                    :test-suffix "_spec"
                                    :test-dir "spec"))

;; Re-add visual-line-mode's fringes
(after! fringe-helper
  (add-to-list 'fringe-indicator-alist
               (list 'continuation
                     (fringe-lib-load fringe-lib-slash)
                     (fringe-lib-load fringe-lib-backslash))))

(after! rspec-mode
  (add-hook! rspec-compilation-mode #'inf-ruby-switch-from-compilation)
  (setq rspec-use-spring-when-possible t)
  (set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :width 0.5 :side 'right :quit 'current))

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

(after! rubocop
  (set-popup-rule! "\\*RuboCop" :ttl 0 :quit 'other))

(after! enh-ruby-mode
  (map! :mode enh-ruby-mode
        (:leader
          (:prefix "p"
            :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))
        (:localleader
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
  (setq lsp-eldoc-render-all nil)
  (setq lsp-eslint-server-command
        '("node"
          "/home/qhuyduong/.vscode/extensions/dbaeumer.vscode-eslint-2.1.1/server/out/eslintServer.js"
          "--stdio"))
  (setq lsp-enable-file-watchers nil))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil))

(after! web-mode
  (setq-hook! 'web-mode-hook
    web-mode-code-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-markup-indent-offset 2
    web-mode-sql-indent-offset 2))

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

(after! gist
  (set-evil-initial-state! 'gist-list-mode 'emacs))

(after! git-gutter
  (setq git-gutter:modified-sign "~"))

(after! doom-themes
  (remove-hook! 'doom-load-theme-hook #'doom-themes-treemacs-config))

(after! buttercup
  (set-popup-rule! "\\*Buttercup\\*" :width 0.5 :side 'right :quit 'current))

(after! projectile-rails
  (set-lookup-handlers! 'projectile-rails-mode :file #'projectile-rails-goto-file-at-point))

(use-package! evil-string-inflection
  :config
  (map! :nv "g~" #'evil-operator-string-inflection))

(use-package! company
  :config
  (setq company-idle-delay 0)
  (add-to-list 'company-backends '(company-files :with company-yasnippet)))

(after! robe
  (set-company-backend! 'enh-ruby-mode '(company-robe company-files :with company-yasnippet)))

(use-package! monkeyc-mode
  :mode "\\.mc\\'")

;;;;;;;;;; Functions ;;;;;;;;;;

(defun projectile-frontend-core-related-files (path)
  (when (string-match "\\(.*\\)\/\\(.*\\)$" path)
    (let* ((dir (match-string 1 path))
           (file-name (match-string 2 path))
           (base-file-name (car (split-string file-name "\\."))))
      (if (projectile-test-file-p file-name)
          (list :impl (concat dir "/../" base-file-name ".js"))
        (list :test (concat dir "/__tests__/" base-file-name ".spec.js"))))))

(dolist (i (number-sequence 0 9))
  (eval `(defun ,(intern (format "+workspace-switch-to-%s" i)) nil
           ,(format "Switch to workspace %s" i)
           (interactive)
           (+workspace/switch-to ,i))))

(defhydra +workspace-hydra (:hint nil)
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

(defun +workspace--tabline-hydra (&optional workspace-names)
  (setq +workspace-hydra/hint
        (let ((names (or workspace-names (+workspace-list-names)))
              (current-name (+workspace-current-name)))
          (mapconcat
           #'identity
           (cl-loop for name in names
                    for i to (length names)
                    collect
                    (propertize (format " [%d] %s " (1+ i) name)
                                'face (if (equal current-name name)
                                          '+workspace-tab-selected-face
                                        '+workspace-tab-face)))
           " ")))
  (+workspace-hydra/body)
  +workspace-hydra/hint)

(advice-add '+workspace--tabline :override #'+workspace--tabline-hydra)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun find-relative-file-or-folder (identifier)
  (let ((path (format "%s/%s" default-directory identifier)))
    (cond ((file-directory-p path)
           (find-file (format "%s/index.js" path)))
          ((file-exists-p path)
           (find-file path))
          (t
           (current-buffer)))))

;;;###autoload
(defun +projectile-rails-goto-template-at-point ()
  "Visit a template or a partial under the point."
  (interactive)
  (require 'find-lisp)
  (let* ((template (projectile-rails-filename-at-point))
         (dir (projectile-rails-template-dir template))
         (name (projectile-rails-template-name template))
         (regex (concat "^[_]?" name)))
    (when (find-lisp-find-files dir regex)
      (find-file (car (find-lisp-find-files dir regex))))))

(advice-add 'projectile-rails-goto-template-at-point :override #'+projectile-rails-goto-template-at-point)

(defun insert-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)
  (random t)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)))))
