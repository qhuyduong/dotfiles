;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Global settings
(setq doom-theme 'doom-material
      doom-localleader-key ","
      display-line-numbers-type nil
      doom-font (font-spec :family "Fira Code" :size 19)
      default-input-method "vietnamese-telex"
      vc-handled-backends '(Git)
      read-process-output-max (* 1024 1024)
      vc-follow-symlinks nil
      system-time-locale "C")

(setq evil-snipe-override-evil-repeat-keys nil)

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

(global-evil-matchit-mode t)

(lsp-treemacs-sync-mode t)

(treemacs-follow-mode t)

(after! dired
  (add-hook! dired-mode #'rspec-dired-mode))

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

  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  (advice-add 'google-translate-json-suggestion :override #'+google-translate-json-suggestion))

;; Keybindings
(map! :nv "C-S-k" #'move-line-up
      :nv "C-S-j" #'move-line-down

      (:leader
       "x" nil ;; Disable x prefix for scratch buffer
       "s" nil ;; Disable s prefix for search

       :desc "search" "/" doom-leader-search-map

       (:prefix "o"
        :desc "List processes" :nv "x" #'list-processes)

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
(map! (:map general-override-mode-map
       :nvim "C-h"  #'evil-window-left
       :nvim "C-j"  #'evil-window-down
       :nvim "C-k"  #'evil-window-up
       :nvim "C-l"  #'evil-window-right)

      (:after ivy :map ivy-switch-buffer-map
       "C-S-v" (general-simulate-key "M-o a v <return>")
       "C-S-s" (general-simulate-key "M-o a s <return>"))
      (:after ivy :map ivy-minibuffer-map
       "C-S-v" (general-simulate-key "M-o a v <return>")
       "C-S-s" (general-simulate-key "M-o a s <return>")))

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
        :map evil-motion-state-map "," nil)

  (defun switch-to-other-window (&optional _count _file)
    (other-window 1))

  (advice-add #'evil-window-split :after #'switch-to-other-window)
  (advice-add #'evil-window-vsplit :after #'switch-to-other-window))

;; Magit
(after! magit
  (setq magit-repository-directories '(("~/workspace" . 1))
        +magit-default-clone-url "git@github.com:%s/%s"))

;; lang/org
(after! org
  (setq org-adapt-indentation nil)
  (setq org-directory (file-name-as-directory "~/org"))
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)
  (setq org-startup-indented nil)
  (setq org-todo-keywords '((sequence "TODO(t!)" "WAITING(w@/!)" "NEXT(n!)" "|" "DONE(d!)" "CANCELED(c@/!)")))
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
  (set-face-attribute 'org-agenda-date-today nil :font (font-spec :family "Fira Code" :size 24) :foreground "lightblue" :underline t)
  (set-popup-rule! "*Calendar*" :width 1 :side 'bottom))

(use-package! apib-mode
  :mode "\\.apib\\'")

(use-package! flycheck-apib
  :when (featurep! :checkers syntax)
  :after apib-mode
  :config (add-hook! apib-mode #'flycheck-apib-setup))

(after! js2-mode
  (setq-default js-indent-level 2)
  (add-hook! js2-mode #'prettier-js-mode))

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
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point))

  (defun counsel-file-vsplit (x)
    (let* ((file (if (and ivy--directory
                          (ivy--dirname-p (ivy-state-current ivy-last)))
                     (substring (ivy-state-current ivy-last) 0 -1)
                   (ivy-state-current ivy-last)))
           (absolute-file (if (projectile-project-p)
                              (expand-file-name file (projectile-project-root))
                            file)))
      (evil-window-vsplit)
      (windmove-right)
      (find-file absolute-file)))

  (defun counsel-file-split (x)
    (let* ((file (if (and ivy--directory
                          (ivy--dirname-p (ivy-state-current ivy-last)))
                     (substring (ivy-state-current ivy-last) 0 -1)
                   (ivy-state-current ivy-last)))
           (absolute-file (if (projectile-project-p)
                              (expand-file-name file (projectile-project-root))
                            file)))
      (evil-window-split)
      (windmove-down)
      (find-file absolute-file)))

  (ivy-add-actions
   'counsel-projectile-find-file
   '(("v" counsel-file-vsplit "open file in vsplit window")
     ("s" counsel-file-split "open file in split window")))
  (ivy-add-actions
   'counsel-find-file
   '(("v" counsel-file-vsplit "open file in vsplit window")
     ("s" counsel-file-split "open file in split window")))

  (defun counsel-projectile-switch-to-buffer-vsplit (x)
    (let ((buffer (if (and ivy--directory
                           (ivy--dirname-p (ivy-state-current ivy-last)))
                      (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (evil-window-vsplit)
      (windmove-right)
      (switch-to-buffer buffer)))

  (defun counsel-projectile-switch-to-buffer-split (x)
    (let ((buffer (if (and ivy--directory
                           (ivy--dirname-p (ivy-state-current ivy-last)))
                      (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (evil-window-split)
      (windmove-down)
      (switch-to-buffer buffer)))

  (ivy-add-actions
   'counsel-projectile-switch-to-buffer
   '(("v" counsel-projectile-switch-to-buffer-vsplit "open buffer in vsplit window")
     ("s" counsel-projectile-switch-to-buffer-split "open buffer in split window")))

  (defun ivy-switch-buffer-vsplit (x)
    (let ((buffer (if (and ivy--directory
                           (ivy--dirname-p (ivy-state-current ivy-last)))
                      (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (if (get-buffer buffer)
          (progn
            (evil-window-vsplit)
            (windmove-right)
            (switch-to-buffer buffer))
        (message "The buffer does not exist"))))

  (defun ivy-switch-buffer-split (x)
    (let ((buffer (if (and ivy--directory
                           (ivy--dirname-p (ivy-state-current ivy-last)))
                      (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (if (get-buffer buffer)
          (progn
            (evil-window-split)
            (windmove-down)
            (switch-to-buffer buffer))
        (message "The buffer does not exist"))))

  (ivy-add-actions
   'ivy-switch-buffer
   '(("v" ivy-switch-buffer-vsplit "open buffer in vsplit window")
     ("s" ivy-switch-buffer-split "open buffer in split window")))

  (defun counsel-rg-file ()
    (->> (ivy-state-current ivy-last)
         (s-split  ":")
         -first-item))

  (defun counsel-rg-split (x)
    (let* ((file (counsel-rg-file))
           (absolute-file (if (projectile-project-p)
                              (expand-file-name file (projectile-project-root))
                            file)))
      (evil-window-split)
      (windmove-down)
      (find-file absolute-file)))

  (defun counsel-rg-vsplit (x)
    (let* ((file (counsel-rg-file))
           (absolute-file (if (projectile-project-p)
                              (expand-file-name file (projectile-project-root))
                            file)))
      (evil-window-vsplit)
      (windmove-right)
      (find-file absolute-file)))

  (ivy-add-actions
   'counsel-rg
   '(("v" counsel-rg-vsplit "open buffer in vsplit window")
     ("s" counsel-rg-split  "open buffer in split window"))))

(after! ruby-mode
  (add-hook! ruby-mode (add-hook 'before-save-hook
                                 (lambda ()
                                   (unless (string= (buffer-name) "schema.rb")
                                     (lsp-format-buffer)))
                                 t t))
  (add-hook! ruby-mode (setq-local flycheck-command-wrapper-function
                                   (lambda (command) (append '("bundle" "exec") command))))
  (setq-default flycheck-disabled-checkers '(ruby-reek))
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
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-enable-file-watchers nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-diagnostics-provider 'flycheck)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local company-backends
                          (cons 'company-files company-backends)))
            t))

(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 1))

(after! lsp-solargraph
  (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby"))

(after! web-mode
  (setq-hook! 'web-mode-hook
    web-mode-code-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-markup-indent-offset 2
    web-mode-sql-indent-offset 2))

(after! css-mode
  (setq css-indent-offset 2)
  (add-hook! css-mode (add-hook 'before-save-hook 'web-beautify-css-buffer t t)))

(after! json-mode
  (add-hook! json-mode (add-hook 'before-save-hook 'json-pretty-print-buffer t t)))

(after! forge
  (setq forge-topic-list-limit '(5 . 5)))

(after! hydra
  (setq hydra-hint-display-type 'lv))

(after! git-gutter
  (setq git-gutter:modified-sign "~"))

(after! company
  (setq company-idle-delay 0))

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

(after! doom-themes
  (setq doom-themes-treemacs-theme 'doom-colors))

(use-package! smali-mode
  :mode "\\.smali\\'")

(after! cc-mode
  (defun linux-kernel-coding-style/c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  ;; Add Linux kernel style
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-add-style "linux-kernel"
                           '("linux" (c-offsets-alist
                                      (arglist-cont-nonempty
                                       c-lineup-gcc-asm-reg
                                       linux-kernel-coding-style/c-lineup-arglist-tabs-only))))))

  (defun linux-kernel-coding-style/setup ()
    (let ((filename (buffer-file-name)))
      ;; Enable kernel mode for the appropriate files
      (when (and filename
                 (or (locate-dominating-file filename "Kbuild")
                     (locate-dominating-file filename "Kconfig")
                     (save-excursion (goto-char 0)
                                     (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))
        (setq indent-tabs-mode t)
        (setq tab-width 8)
        (setq c-basic-offset 8)
        (c-set-style "linux-kernel")
        (message "Setting up indentation for the linux kernel"))))

  (add-hook 'c-mode-hook 'linux-kernel-coding-style/setup))

(use-package! multi-vterm
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
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

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
