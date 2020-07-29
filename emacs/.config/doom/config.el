;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Global settings
(setq doom-theme 'doom-palenight
      doom-localleader-key ","
      display-line-numbers-type nil
      initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
      doom-font (font-spec :family "Fira Code" :size 14)
      default-input-method "vietnamese-telex"
      vc-handled-backends '(Git)
      read-process-output-max (* 1024 1024)
      fancy-splash-image "~/.config/doom/black-hole.png"
      alert-default-style 'libnotify
      vc-follow-symlinks nil
      find-file-visit-truename nil)

;; Make Emacs fullscreen by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Emacs sometimes registers C-s-f as this weird keycode
(global-set-key (kbd "<C-S-s>") #'toggle-frame-fullscreen)

;; Workaround for magithub authentication stuffs
(setq auth-sources '("~/.authinfo"))

;; truncate-lines in all buffers
(setq-default truncate-lines nil
              global-visual-line-mode t)

;; Workaround for magithub authentication stuffs
(add-to-list 'auth-sources "~/.authinfo")

(setq delete-by-moving-to-trash t)

(global-evil-matchit-mode t)

(after! doom-modeline
  (setq doom-modeline-percent-position nil))

(after! dired
  (add-hook! dired-mode #'rspec-dired-mode)
  (map! :mode dired-mode
        [remap dired-find-file] #'dired-single-buffer
        [remap dired-up-directory] #'dired-single-up-directory))

;; Set default source and destination languages for Google Translate
(after! google-translate-core-ui
  (setq google-translate-backend-method 'curl)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi")
  (setq google-translate-show-phonetic t)

  (defun +google-translate-json-suggestion (json)
    "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1)
        nil)))

  (advice-add 'google-translate-json-suggestion :override #'+google-translate-json-suggestion))

;; Keybindings
(map! :nv "C-S-k" #'move-line-up
      :nv "C-S-j" #'move-line-down

      (:leader
       "x" nil ;; Disable x prefix for scratch buffer
       "s" nil ;; Disable s prefix for search

       :desc "search" "/" doom-leader-search-map

       (:prefix "o"
        :desc "List processes" :nv "x" #'list-processes
        (:prefix "a"
         :desc "Getting things done" :nv "g" #'org-agenda-gtd))

       (:prefix "p"
        :desc "Find dir" :nv "d" #'counsel-projectile-find-dir)

       (:prefix "i"
        :desc "UUIDv4" :n "u" #'insert-random-uuid
        (:prefix ("l" . "lorem-ipsum")
         :desc "list" :n "l" #'lorem-ipsum-insert-list
         :desc "sentences" :n "s" #'lorem-ipsum-insert-sentences
         :desc "paragraphs" :n "p" #'lorem-ipsum-insert-paragraphs))

       (:prefix ("x" . "text-transform")
        :desc "Translate this text" :nv "g" #'google-translate-at-point
        (:prefix ("f" . "copy-as-format")
         :desc "Github" :nv "g" #'copy-as-format-github
         :desc "HTML" :nv "h" #'copy-as-format-html
         :desc "Markdown" :nv "m" #'copy-as-format-markdown
         :desc "Org" :nv "o" #'copy-as-format-org
         :desc "Slack" :nv "s" #'copy-as-format-slack))))

;; Easier window navigation
(map! :map general-override-mode-map
      :nvim "C-h"  #'evil-window-left
      :nvim "C-j"  #'evil-window-down
      :nvim "C-k"  #'evil-window-up
      :nvim "C-l"  #'evil-window-right)

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
  (setq magit-git-executable "/usr/local/bin/git")
  (setq magit-repository-directories '(("~/workspace" . 1))
        +magit-default-clone-url "git@github.com:%s/%s"))

;; lang/org
(after! org
  (setq org-adapt-indentation nil)
  (setq org-directory (file-name-as-directory "~/org"))
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)
  (setq org-startup-indented nil)
  (setq org-gtd-gtd-file (concat org-directory "gtd.org"))
  (setq org-gtd-tickler-file (concat org-directory "tickler.org"))
  (setq org-gtd-inbox-file (concat org-directory "inbox.org"))
  (setq org-gtd-someday-file (concat org-directory "someday.org"))
  (setq org-agenda-files (list org-gtd-gtd-file org-gtd-inbox-file org-gtd-tickler-file))
  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file ,org-gtd-inbox-file)
                                 "\
* TODO %i%?
:PROPERTIES:
:CREATED: %U
:END:")
                                ("k" "Tickler" entry
                                 (file ,org-gtd-tickler-file)
                                 "\
* %i%?
%^t
:PROPERTIES:
:CREATED: %U
:END:")))
  (setq org-refile-targets `((,org-gtd-gtd-file :maxlevel . 3)
                             (,org-gtd-someday-file :level . 1)
                             (,org-gtd-tickler-file :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces '(("TODO" . (:foreground "grey"))
                                 ("WAITING" . (:foreground "yellow"))
                                 ("NEXT" . (:foreground "lightblue"))
                                 ("DONE" . (:foreground "green"))
                                 ("CANCELED" . (:foreground "red"))))
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil)
  (setq org-tag-alist '(("@work" . ?w) ("@personal" . ?p)))
  (set-face-attribute 'org-agenda-date-today nil :font (font-spec :family "Fira Code" :size 24) :foreground "lightblue" :underline t)
  (setq org-protocol-default-template-key "t")
  (map! :map org-super-agenda-header-map
        "j" #'evil-next-line
        "k" #'evil-previous-line)
  (set-popup-rule! "*Calendar*" :width 1 :side 'bottom)
  (org-super-agenda-mode t)
  (setq org-agenda-custom-commands
        `(("g" "Getting things done"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Important"
                             :priority "A"
                             :order 2)
                            (:name "Next to do"
                             :todo "NEXT"
                             :order 5)
                            (:name "Waiting"
                             :todo "WAITING"
                             :order 40)
                            (:name "Due Today"
                             :deadline today
                             :order 2)))
                         (org-agenda-files `(,org-gtd-gtd-file))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Inbox"
                             :anything)))
                         (org-agenda-files `(,org-gtd-inbox-file)))))))))

;; apib-mode
(use-package! apib-mode
  :mode "\\.apib\\'")

;; flycheck-apib
(use-package! flycheck-apib
  :when (featurep! :checkers syntax)
  :after apib-mode
  :config (add-hook! apib-mode #'flycheck-apib-setup))

;; js2-mode
(after! js2-mode
  (setq-default js-indent-level 2)
  (map! :mode js2-mode
        (:leader
         (:prefix "p"
          :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))))

(after! tide
  (setq tide-native-json-parsing t))

;; projectile
(after! projectile
  (defun projectile-frontend-core-related-files (path)
    (when (string-match "\\(.*\\)\/\\(.*\\)$" path)
      (let* ((dir (match-string 1 path))
             (file-name (match-string 2 path))
             (base-file-name (car (split-string file-name "\\."))))
        (if (projectile-test-file-p file-name)
            (list :impl (concat dir "/../" base-file-name ".js"))
          (list :test (concat dir "/__tests__/" base-file-name ".spec.js"))))))

  (setq projectile-create-missing-test-files t)
  ;; Configure npm project with projectile
  (projectile-register-project-type 'npm '("package.json")
                                    :test "yarn test"
                                    :test-suffix ".spec"
                                    :related-files-fn #'projectile-frontend-core-related-files)
  ;; Workaround for Rails 6
  (projectile-register-project-type 'rails-rspec '("Gemfile")
                                    :src-dir "app"
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
  (set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :width 0.25 :side 'right :quit 'current))

(after! ivy
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)))

(after! ruby-mode
  (setq-hook! 'ruby-mode-hook
    flycheck-command-wrapper-function (lambda (command)
                                        (append '("bundle" "exec") command)))
  (map! :mode ruby-mode
        (:leader
         (:prefix "p"
          :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))
        (:localleader
         (:prefix ("b" . "bundle"))
         (:prefix ("k" . "rake"))
         (:prefix ("r" . "robe"))
         (:prefix ("s" . "inf-ruby"))
         (:prefix ("t" . "rspec")))))

(after! lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local company-backends
                          (cons 'company-files company-backends)))
            t)
  (setq lsp-eldoc-enable-hover nil
        lsp-enable-file-watchers nil
        lsp-prefer-capf t
        lsp-diagnostic-package :none))

(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 1))

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
  (setq hydra-hint-display-type 'lv))

(after! git-gutter
  (setq git-gutter:modified-sign "~"))

(after! doom-themes
  (remove-hook! 'doom-load-theme-hook #'doom-themes-treemacs-config))

(after! projectile-rails
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

  (set-lookup-handlers! 'projectile-rails-mode :file #'projectile-rails-goto-file-at-point))

(after! evil-string-inflection
  (map! :nv "g~" #'evil-operator-string-inflection))

(after! company
  (setq company-idle-delay 0))

(after! plantuml-mode
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))

(after! emmet-mode
  (setq emmet-expand-jsx-className? t))

(after! rjsx-mode
  (defun +rjsx-electric-gt (_)
    (when (and (looking-back "<>")
               (looking-at-p "/>"))
      (save-excursion (insert "<"))))

  (advice-add #'rjsx-electric-gt :after #'+rjsx-electric-gt))

(after! company-files
  (defun +company-files--post-completion (arg)
    (let* ((file-name-regex (rx (and "." (or "js" "jsx" "rb") line-end)))
           (matched-position (string-match file-name-regex arg)))
      (when matched-position (delete-char (- (- (length arg) matched-position))))))

  (advice-add #'company-files--post-completion :after #'+company-files--post-completion))

(after! smerge-mode
  (defhydra ++vc/smerge-hydra (:hint nil
                               :pre (if (not smerge-mode) (smerge-mode 1))
                               ;; Disable `smerge-mode' when quitting hydra if
                               ;; no merge conflicts remain.
                               :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_N_^       [_m_] mine       [_=_] upper/lower   [_r_] resolve
     ^_C-u_ ↑↑^  [_o_] other      [_>_] base/lower    [_R_] remove
     ^_k_ ↑^     [_c_] current    [_H_] hightlight    [_u_] undo
     ^_j_ ↓^     [_a_] all        [_E_] ediff         [_C-r_] redo
     ^_C-d_ ↓↓^
     ^_n_^                                                ╭──────────
     ^_G_^                                                │ [_q_] quit
"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("n" smerge-next)
    ("N" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("C-u" evil-scroll-up)
    ("C-d" evil-scroll-down)
    ("b" smerge-keep-base)
    ("m" smerge-keep-upper)
    ("o" smerge-keep-lower)
    ("c" smerge-keep-current)
    ("a" smerge-keep-all)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("u" undo-fu-only-undo)
    ("C-r" undo-fu-only-redo)
    ("q" nil :color blue))

  (advice-add '+vc/smerge-hydra/body :override #'++vc/smerge-hydra/body))


(after! org-caldav
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq org-caldav-url 'google)
  (setq org-caldav-calendar-id (getenv "GCAL_TICKLER_CALENDAR_ID"))
  (setq org-caldav-oauth2-client-id (getenv "GCAL_CLIENT_ID"))
  (setq org-caldav-oauth2-client-secret (getenv "GCAL_CLIENT_SECRET"))
  (setq org-caldav-inbox org-gtd-tickler-file)

  (defun +org-caldav-sync-after-capture ()
    "Sync calendar after a event was added with org-capture.
The function can be run automatically with the 'org-capture-after-finalize-hook'."
    (when (string= (org-capture-get :key) "k")
      (org-caldav-sync)))

  (add-hook 'org-capture-after-finalize-hook '+org-caldav-sync-after-capture))

(use-package! hl-line+
  :load-path "."
  :config
  (hl-line-when-idle-interval 0.3)
  (toggle-hl-line-when-idle 1))

(after! centaur-tabs
  (map! :nvim "<C-tab>" #'centaur-tabs-forward
        :nvim "<C-S-tab>" #'centaur-tabs-backward
        :nvim "<C-S-iso-lefttab>" #'centaur-tabs-backward
        :nvim "C-," #'centaur-tabs-move-current-tab-to-left
        :nvim "C-." #'centaur-tabs-move-current-tab-to-right))

;;;;;;;;;; Functions ;;;;;;;;;;

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

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

(defun org-agenda-gtd ()
  (interactive)
  (org-agenda nil "g"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#98f5ff")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval flycheck-cask-setup))))
