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

(setq +workspaces-switch-project-function #'+counsel-fzf-find-project)

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
(projectile-rails-global-mode)

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
        :nv "*" nil
        :nv "s" nil

        (:prefix "o"
          :desc "List processes" :nv "x" #'list-processes
          :desc "Project sidebar" :nv "p" #'treemacs
          :desc "Toggle vterm popup" :nv "t" #'multi-libvterm-projectile)

        (:prefix "w"
          :nv "d"  #'evil-window-delete)

        (:prefix "/"
          :desc "Search buffer"                    "b" #'swiper
          :desc "Search current directory"         "d" #'+default/search-cwd
          :desc "Search other directory"           "D" #'+default/search-other-cwd
          :desc "Locate file"                      "f" #'locate
          :desc "Jump to symbol"                   "i" #'imenu
          :desc "Jump to link"                     "l" #'ace-link
          :desc "Jump list"                        "j" #'evil-show-jumps
          :desc "Jump to mark"                     "m" #'evil-show-marks
          :desc "Look up online"                   "o" #'+lookup/online
          :desc "Look up online (w/ prompt)"       "O" #'+lookup/online-select
          :desc "Look up in local docsets"         "k" #'+lookup/in-docsets
          :desc "Look up in all docsets"           "K" #'+lookup/in-all-docsets
          :desc "Search project"                   "p" #'+default/search-project
          :desc "Search other project"             "P" #'+default/search-other-project
          :desc "Search buffer"                    "s" #'swiper-isearch
          :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point
          :desc "Search for symbol in project"     "*" #'+default/search-project-for-symbol-at-point)

        (:prefix "p"
          :desc "Find dir" :nv "d" #'counsel-projectile-find-dir
          :desc "Find file in project" :nv "/" #'+counsel-fzf-find-project)

        (:prefix "g"
          :desc "Resolve conflicts" :n "r" #'smerge-hydra/body)

        (:prefix "w"
          :desc "evil-window-resize" :n "r" #'evil-window-resize-hydra/body)

        (:prefix "i"
          :desc "UUIDv4" :n "u" #'insert-random-uuid
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
  (map! "C-c c" #'org-capture
        "C-c n p" #'org-projectile-project-todo-completing-read
        :nv "C-c C-p" #'org-pomodoro)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (add-to-list 'org-modules 'org-drill)
  (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)
  (setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")
  (remove-hook! org-mode #'org-indent-mode)
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
  (add-hook! rjsx-mode #'(add-node-modules-path prettier-js-mode tern-mode))
  (add-hook! rjsx-mode (add-hook '+lookup-file-functions #'find-relative-file-or-folder nil t))
  (set-company-backend! 'rjsx-mode '(company-tern company-files :with company-yasnippet))
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
  (setq lsp-eldoc-render-all nil))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-doc-enable nil))

(after! web-mode
  (setq-hook! 'web-mode-hook
    web-mode-code-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-markup-indent-offset 2
    web-mode-sql-indent-offset 2)
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

(after! gist
  (set-evil-initial-state! 'gist-list-mode 'emacs))

(after! git-gutter
  (setq git-gutter:modified-sign "~"))

(after! doom-themes
  (remove-hook! 'doom-load-theme-hook #'doom-themes-treemacs-config))

(after! buttercup
  (set-popup-rule! "\\*Buttercup\\*" :width 0.3 :side 'right :quit 'current))

(after! projectile-rails
  (set-lookup-handlers! 'projectile-rails-mode :file #'projectile-rails-goto-file-at-point))

(use-package! evil-string-inflection
  :config
  (map! :nv "g~" #'evil-operator-string-inflection))

(use-package! tern
  :config
  (set-lookup-handlers! 'tern-mode :definition '(tern-find-definition :async t)))

(use-package! company
  :config
  (setq company-idle-delay 0)
  (add-to-list 'company-backends '(company-files :with company-yasnippet)))

(after! robe
  (set-company-backend! 'enh-ruby-mode '(company-robe company-files :with company-yasnippet)))

(use-package! vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-libvterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-libvterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-libvterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;;;;;;;;;; Hydras ;;;;;;;;;;

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
%s(+workspace--tabline)"
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

;;;;;;;;;; Functions ;;;;;;;;;;

(defun +counsel-fzf-find-project (&optional dir)
  (interactive)
  (let ((project-dir (if dir dir (projectile-project-root))))
    (counsel-fzf nil dir (format "[%s] " (file-name-nondirectory (directory-file-name project-dir))))))

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
(defun ++lookup/file (path)
  "Figure out PATH from whatever is at point and open it.

Each function in `+lookup-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive
   (progn
     (require 'ffap)
     (list
      (or (ffap-guesser)
          (+lookup-symbol-or-region)))))
  (require 'ffap)
  (cond ((not path)
         (call-interactively #'find-file-at-point))

        ((ffap-url-p path)
         (find-file-at-point path))

        ((not (+lookup--jump-to :file path))
         (let ((fullpath (doom-path path)))
           (when (and buffer-file-name (file-equal-p fullpath buffer-file-name))
             (user-error "Already here"))
           (let* ((insert-default-directory t)
                  (project-root (doom-project-root))
                  (ffap-file-finder
                   (cond ((not (doom-glob fullpath))
                          #'find-file)
                         ((ignore-errors (file-in-directory-p fullpath project-root))
                          (lambda (dir)
                            (let* ((default-directory dir)
                                   projectile-project-name
                                   projectile-project-root
                                   (projectile-project-root-cache (make-hash-table :test 'equal))
                                   (file (projectile-completing-read "Find file: "
                                                                     (projectile-current-project-files)
                                                                     :initial-input path)))
                              (find-file (expand-file-name file (doom-project-root)))
                              (run-hooks 'projectile-find-file-hook))))
                         (#'doom-project-browse))))
             (find-file-at-point path))))))

(advice-add '+lookup/file :override #'++lookup/file)

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

(add-hook '+lookup-file-functions #'current-buffer)
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

(evil-define-command +evil-paste-and-indent-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command #'evil-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (evil-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (push-mark opoint t)
            (dotimes (i (or count 1))
              (insert-for-yank text))
            (setq evil-last-paste
                  (list #'evil-paste-before
                        count
                        opoint
                        opoint    ; beg
                        (point))) ; end
            (evil-set-marker ?\[ opoint)
            (evil-set-marker ?\] (1- (point)))
            (when (and evil-move-cursor-back
                       (> (length text) 0))
              (backward-char))))
        ;; no paste-pop after pasting from a register
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))
    (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))))

(evil-define-command +evil-paste-and-indent-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command #'evil-paste-after))
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (evil-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (i (or count 1))
                (insert-for-yank text))
              (setq evil-last-paste
                    (list #'evil-paste-after
                          count
                          opoint
                          beg       ; beg
                          (point))) ; end
              (evil-set-marker ?\[ beg)
              (evil-set-marker ?\] (1- (point)))
              (when (evil-normal-state-p)
                (evil-move-cursor-back)))))
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text))
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))

(evil-define-command +evil-visual-paste-and-indent (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (evil-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (evil-visual-state-p)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete evil-visual-beginning evil-visual-end
                       (evil-visual-type))
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (eq (evil-visual-type) 'line))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (evil-paste-after count register)
          (evil-paste-before count register)))
      (when evil-kill-on-visual-paste
        (kill-new new-kill))
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))
    (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))))

;; (advice-add #'evil-paste-before :override #'+evil-paste-and-indent-before)
;; (advice-add #'evil-paste-after :override #'+evil-paste-and-indent-after)
;; (advice-add #'evil-visual-paste :override #'+evil-visual-paste-and-indent)
