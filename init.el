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
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker   ; tasing you for every semicolon you forget
        +childframe)     ; use childframes for error popups (Emacs 26+ only)
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)     ; enables displaying completion candidates in a child frame
       (helm             ; the *other* search engine for love and life
        +fuzzy           ; enables fuzzy search backend for helm
        +childframe)     ; causes helm to display in a floating child frame

       :ui
       doom              ; what makes DOOM look the way it does
       evil-goggles      ; display visual hints when editing in evil
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       modeline          ; snazzy, Atom-inspired modeline, plus API
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       treemacs          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :emacs
       (dired            ; making dired pretty [functional]
        +ranger          ; bringing the goodness of ranger to dired
        +icons)          ; colorful icons for dired-mode
       electric          ; smarter, keyword-based electric-indent
       imenu             ; an imenu sidebar and searchable code index
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       macos             ; MacOS-specific commands
       (magit            ; a git porcelain for Emacs
        +forge)          ; github integration to magit

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +babel           ; running code in org
        +capture)        ; org-capture in and outside of Emacs
       (ruby             ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +rbenv)          ; enable rbenv
       web               ; the tubes

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +snippets +evil-commands))
