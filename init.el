;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature
       eval              ; run code, run (also, repls)
       (evil             ; come to the dark side, we have cookies
        +everywhere)     ; enables evil globally
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to

       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)     ; enables displaying completion candidates in a child frame
       (ivy              ; a search engine for love and life
        +fuzzy)          ; enables fuzzy search backend for ivy

       :ui
       doom              ; what makes DOOM look the way it does
       evil-goggles      ; display visual hints when editing in evil
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       treemacs          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe

       :editor
       multiple-cursors  ; editing in many places at once

       :emacs
       (dired            ; making dired pretty [functional]
        +icons)          ; colorful icons for dired-mode
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       (flycheck         ; tasing you for every semicolon you forget
        +childframe)     ; use childframes for error popups (Emacs 26+ only)
       lsp               ; LSP client
       macos             ; MacOS-specific commands
       (magit            ; a git porcelain for Emacs
        +forge)          ; github integration to magit
       vterm             ; another terminals in Emacs

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +present)        ; Emacs for presentations
       (ruby             ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +rbenv           ; enable rbenv
        +lsp)            ; enable LSP
       web               ; the tubes

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens))
