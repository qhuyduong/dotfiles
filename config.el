;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Emacs GUI settings
(setq doom-font (font-spec :family "Iosevka SS07" :size 15 :weight 'light))
(setq doom-theme 'doom-palenight)
(setq doom-localleader-key ",")
(setq display-line-numbers-type 'relative)
(setq line-number-mode nil)
(setq initial-scratch-message ";; Happy Hacking")

(custom-set-faces '(cursor ((t (:background "#98f5ff")))))

(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

;; truncate-lines in all buffers
(setq-default truncate-lines nil)
(setq-default global-visual-line-mode t)

;; Make titlebar match background color
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Make Emacs fullscreen by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Workaround for magithub authentication stuffs
(setq auth-sources '("~/.authinfo"))

;; Set command key as super on OSX
(setq mac-command-modifier 'super)

;; Set option key as meta on OSX
(setq mac-option-modifier 'meta)

(setq delete-by-moving-to-trash t)

(after! doom-modeline
  (remove-hook! 'doom-modeline-mode-hook #'column-number-mode)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-percent-position nil))

;; Reduce which-key delay
(after! which-key
  (setq which-key-idle-delay 0.5))

(after! osx-trash
  (when (eq system-type 'darwin)
    (osx-trash-setup)))

(after! dired
  ;; Icons in dired
  (require 'font-lock+)
  (map! :mode dired-mode
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

      ;; Easier window navigation
      :nvi "C-h"  #'evil-window-left
      :nvi "C-j"  #'evil-window-down
      :nvi "C-k"  #'evil-window-up
      :nvi "C-l"  #'evil-window-right
      :nv "C-S-k" #'move-line-up
      :nv "C-S-j" #'move-line-down

      (:leader
        :nv "x" nil ;; Disable x prefix for scratch buffer

        (:prefix "o"
          :desc "List processes" :nv "x"  #'list-processes
          :desc "Terminal" :nv "t" #'vterm/open-in-project
          :desc "Project sidebar" :nv "p"  #'treemacs
          :desc "Prodigy" :nv "s"  #'prodigy)

        (:prefix "b"
          :nv "s" nil
          :nv "k" nil
          :desc "Delete buffer" :nv "d"  #'kill-this-buffer)

        (:prefix "w"
          :nv "c" nil
          :nv "d"  #'evil-window-delete)

        (:prefix "/"
          :desc "Search this text in project" :nv "*"  #'counsel-projectile-ag)

        (:prefix "p"
          :desc "Find dir" :nv "d" #'counsel-projectile-find-dir)

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
            :desc "Slack" :nv "s" #'copy-as-format-slack))))

;; Emacs sometimes registers C-s-f as this weird keycode
(global-set-key (kbd "<C-s-268632070>") #'toggle-frame-fullscreen)

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
  (setq magit-status-mode-hook nil)
  (setq-hook! 'magit-status-mode-hook header-line-format '(:eval (tabbar-line)))
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
  :when (featurep! :feature syntax-checker)
  :after apib-mode
  :config (add-hook! apib-mode #'flycheck-apib-setup))

;; apib-mode
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; js2-mode
(after! js2-mode
  (setq-default js-indent-level 2)
  (add-hook! js2-mode #'(add-node-modules-path prettier-js-mode))
  (add-hook! js2-mode (nvm-use "10.15.0"))
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
  (add-hook! rspec-compilation-mode #'inf-ruby-switch-setup)
  (set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :width 0.3 :side 'right :quit 'current)
  ;; Rspec doesn't use RVM!
  (setq rspec-use-rvm nil))

(after! treemacs
  (map! :mode treemacs-mode
        "C-h"  #'evil-window-left
        "C-j"  #'evil-window-down
        "C-k"  #'evil-window-up
        "C-l"  #'evil-window-right)
  (setq treemacs-show-cursor t)
  (setq doom-treemacs-use-generic-icons nil)
  (treemacs-follow-mode t))

(after! helm
  (setq helm-ag-insert-at-point 'symbol)
  (setq +helm-posframe-text-scale nil))

(after! ivy
  (set-face-foreground 'ivy-current-match "#c3e88d")
  (setq counsel-projectile-ag-initial-input '(ivy-thing-at-point)))

(after! enh-ruby-mode
  (map! :mode enh-ruby-mode
        (:leader
          (:prefix "p"
            :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))
        (:localleader
          (:desc "Toggle block" "}" #'enh-ruby-toggle-block)
          (:prefix ("l" . "lsp")
            "f" #'lsp-format-buffer)
          (:prefix ("b" . "bundle"))
          (:prefix ("k" . "rake"))
          (:prefix ("r" . "robe"))
          (:prefix ("s" . "inf-ruby"))
          (:prefix ("t" . "rspec")))))

(after! all-the-icons
  (add-to-list 'all-the-icons-mode-icon-alist
               '(enh-ruby-mode all-the-icons-alltheicon "ruby-alt" :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2)))

(after! org-projectile
  (setq org-projectile-projects-file "~/Workspace/orgs/inbox.org")
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(after! reason-mode
  (add-hook! reason-mode #'lsp)
  (add-hook! reason-mode (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                    :major-modes '(ruby-mode enh-ruby-mode)
                    :priority 1
                    :initialization-options '(:diagnostics t)
                    :server-id 'enh-ruby-ls))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))
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

(after! vterm
  (add-hook! vterm-mode (setq-local evil-insert-state-cursor 'box))
  (map! :mode vterm-mode
        :i [return] #'vterm-send-return
        :i "C-c" #'vterm--self-insert
        :i "C-h" #'vterm--self-insert))

(after! forge
  (setq forge-topic-list-limit '(5 . 5)))

(after! hydra
  (setq hydra-hint-display-type 'message))

(after! prodigy
  (setq prodigy-view-buffer-maximum-size 512)
  (add-hook! (prodigy-mode prodigy-view-mode) (nvm-use "10.15.0"))
  (set-evil-initial-state! 'prodigy-mode 'normal)
  (map! :mode prodigy-mode
        (:prefix "g"
          :nv "s" #'prodigy-start
          :nv "S" #'prodigy-stop)))

(after! tide
  (setq tide-hl-identifier-idle-time 2)
  (setq tide-sync-request-timeout 5))

(tabbar-mode t)

(after! tabbar
  (require 'tabbar-ruler)

  (setq tabbar-scroll-right-button '(("") ""))
  (setq tabbar-scroll-left-button '(("") ""))
  (setq tabbar-buffer-home-button '(("") ""))
  (setq tabbar-background-color "#3e4151")
  (setq tabbar-buffer-list-function #'+tabbar-buffer-list)
  (setq tabbar-buffer-groups-function #'+tabbar-buffer-groups)
  (custom-set-faces '(tabbar-selected ((t (:background "#292d3e" :slant italic :weight medium))))
                    '(tabbar-selected-modified ((t (:foreground "#ff5370"))))
                    '(tabbar-unselected ((t (:foreground "#EEFFFF" :background "#3e4151"))))
                    '(tabbar-unselected-modified ((t (:background "#3e4151")))))

  (map! "s-]" #'tabbar-forward-tab
        "s-[" #'tabbar-backward-tab
        "s-}" #'+tabbar-forward-group
        "s-{" #'+tabbar-backward-group

        (:leader
          (:prefix ("TAB" . "tabbar")
            :desc "Forward tab group" :nv "]" #'+tabbar-forward-group
            :desc "Backward tab group" :nv "[" #'+tabbar-backward-group
            :desc "Kill all buffers in group" :nv "d" #'+tabbar-kill-all-buffers-in-current-group
            :desc "Kill other buffers in group" :nv "D" #'+tabbar-kill-other-buffers-in-current-group
            :desc "Switch to tab group" :nv "TAB" #'+tabbar-group-hydra/body))))

(after! tabbar-ruler
  (setq tabbar-ruler-global-tabbar t)
  (setq tabbar-ruler-fancy-close-image t)
  (setq tabbar-ruler-fancy-tab-separator 'box)
  (setq tabbar-ruler-fancy-current-tab-separator 'box))

(after! mode-icons
  (add-to-list 'mode-icons '("\\`JS2\\'" "js" xpm))
  (add-to-list 'mode-icons '("\\`Elisp\\'" "emacs" xpm))
  (add-to-list 'mode-icons '("\\`VTerm\\'" "term" xpm))
  (add-to-list 'mode-icons '("\\`JSON\\'" #xe90b all-the-icons))
  (add-to-list 'mode-icons '("\\`BSDmakefile\\'" #xe679 file-icons))
  (add-to-list 'mode-icons '("\\`Gitignore\\'" #xf1d2 FontAwesome)))

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

(defun +tabbar-forward-group ()
  (interactive)
  (tabbar-forward-group)
  (minibuffer-message "Tabbar Group: %s" (cdr (tabbar-selected-tab (tabbar-current-tabset t)))))

(defun +tabbar-backward-group ()
  (interactive)
  (tabbar-backward-group)
  (minibuffer-message "Tabbar Group: %s" (cdr (tabbar-selected-tab (tabbar-current-tabset t)))))

(defun +tabbar-hide-tab (buffer)
  (let ((name (format "%s" buffer)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name begin with asterisk *
     (and (string-match-p "^[ ]*\\*" name)
          ;; but not one of these buffers
          (not (or (string-prefix-p "*rspec" name)
                   (string-prefix-p "*prodigy-" name))))

     (string-match-p "treemacs-persist" name)

     ;; Is not magit-* buffer (except magit source file)
     (and (string-prefix-p "magit-" name)
          (not (string= (file-name-extension name) "el"))))))

(defun +tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or *, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (buffer)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) buffer) buffer)
                     ((buffer-file-name buffer) buffer)
                     ;; Buffer name begin with asterisk *
                     ((+tabbar-hide-tab buffer) nil)
                     ((buffer-live-p buffer) buffer)))
                (buffer-list))))

(defun +tabbar-buffer-groups ()
  "Group priority:
1. Handle some exceptional buffers (e.g: Prodigy)
2. Try to add buffer to project.
3. Group buffer with mode if buffer is derived from `dired-mode' `org-mode'.
4. Other buffers pushed to group \"Emacs\"."
  (let ((current-project (cdr (project-current))))
    (list
     (cond
      ((string-prefix-p "*prodigy-" (buffer-name))
       "Prodigy")
      (current-project
       current-project)
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      (t "Emacs")))))

(defmacro +tabbar-kill-buffer-match-rule (match-rule)
  `(save-excursion
     (mapc #'(lambda (buffer)
               (with-current-buffer buffer
                 (when (string-equal current-group-name
                                     (cdr (tabbar-selected-tab (tabbar-current-tabset t))))
                   (when (funcall ,match-rule buffer)
                     (kill-buffer buffer))
                   )))
           (buffer-list))))

(defun +tabbar-kill-all-buffers-in-current-group ()
  "Kill all buffers in current group."
  (interactive)
  (let* ((current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t)))))
    ;; Kill all buffers in current group.
    (+tabbar-kill-buffer-match-rule (lambda (_buffer) t))
    ;; Switch to next group.
    (+tabbar-forward-group)))

(defun +tabbar-kill-other-buffers-in-current-group ()
  "Kill all buffers except current buffer in current group."
  (interactive)
  (let* ((current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t))))
         (current-buffer (current-buffer)))
    ;; Kill all buffers in current group.
    (+tabbar-kill-buffer-match-rule (lambda (buffer) (not (equal buffer current-buffer))))))

(defun vterm-send-escape ()
  (interactive)
  (vterm-send-string "\e"))

(defun vterm-send-return ()
  "Sends C-m to the libvterm."
  (interactive)
  (process-send-string vterm--process "\C-m"))

(defun vterm/open-in-project ()
  (interactive)
  (+vterm/open t))

(when (file-exists-p "~/.doom.d/+prodigy-services.el")
  (load! "+prodigy-services"))

(defun projectile-frontend-core-related-files (path)
  (when (string-match "\\(.*\\)\/\\(.*\\)$" path)
    (let* ((dir (match-string 1 path))
           (file-name (match-string 2 path))
           (base-file-name (car (split-string file-name "\\."))))
      (if (projectile-test-file-p file-name)
          (list :impl (concat dir "/../" base-file-name ".js"))
        (list :test (concat dir "/__tests__/" base-file-name ".spec.js"))))))

(defun +tabbar-get-groups ()
  "Get all tab groups"
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab))
  (mapcar #'(lambda (group)
              (format "%s" (cdr group)))
          (tabbar-tabs tabbar-tabsets-tabset)))

(defun +tabbar-switch-group (&optional groupname)
  "Switch tab groups using ido."
  (interactive)
  (let* ((tab-buffer-list (mapcar
                           #'(lambda (b)
                               (with-current-buffer b
                                 (list (current-buffer)
                                       (buffer-name)
                                       (funcall tabbar-buffer-groups-function) )))
                           (funcall tabbar-buffer-list-function)))
         (groups (+tabbar-get-groups))
         (group-name (or groupname (completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (car (car (cdr (cdr group)))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))

(defun +tabbar-switch-group-by-pos (pos)
  "Switch to tab group of position POS."
  (let ((group-to-switch (nth pos (+tabbar-get-groups))))
    (if group-to-switch
        (+tabbar-switch-group group-to-switch))))

(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "+tabbar-switch-group-to-%s" i)) nil
           ,(format "Switch to tab group %s.\n%s"
                    i "See `+tabbar-switch-group-by-pos' for details.")
           (interactive)
           (+tabbar-switch-group-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun +tabbar-group-tab-layout ()
  "Layout for display tab groups"
  (let* ((group-current-name (tabbar-current-tabset t))
         (highlight-groups (lambda (elt idx)
                             (if (string= elt group-current-name)
                                 (propertize
                                  (format "%d:%s" (+ idx 1) (f-base elt))
                                  'face '(:foreground "red" :background "yellow"))
                               (format "%d:%s" (+ idx 1) (f-base elt))))))
    (message "%s" highlight-groups)
    (string-join (seq-map-indexed highlight-groups (+tabbar-get-groups)) " | ")))

(defhydra +tabbar-group-hydra (:hint nil :exit t)
  "
Tab groups: %s(+tabbar-group-tab-layout)
"
  ("n" +tabbar-forward-group "Next group")
  ("p" +tabbar-backward-group "Prev group")
  ("l" +tabbar-switch-group "Switch group")
  ("0" +tabbar-switch-group-to-0)
  ("1" +tabbar-switch-group-to-1)
  ("2" +tabbar-switch-group-to-2)
  ("3" +tabbar-switch-group-to-3)
  ("4" +tabbar-switch-group-to-4)
  ("5" +tabbar-switch-group-to-5)
  ("6" +tabbar-switch-group-to-6)
  ("7" +tabbar-switch-group-to-7)
  ("8" +tabbar-switch-group-to-8)
  ("9" +tabbar-switch-group-to-9)
  ("q" nil "quit"))
