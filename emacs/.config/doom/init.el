;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's
;;      modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       (company          ; the ultimate code completion backend
        +childframe)
       (ivy              ; a search engine for love and life
        +icons)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       tabs              ; an tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired            ; making dired pretty [functional]
        +icons)          ; colorful icons for dired-mode
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax           ; tasing you for every semicolon you forget
        +childframe)
       spell             ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       docker
       (eval             ; run code, run (also, repls)
         +overlay)
       lookup            ; helps you navigate your code and documentation
       lsp
       (magit            ; a git porcelain for Emacs
        +forge)
       pdf               ; pdf enhancements

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (javascript       ; all(hope(abandon(ye(who(enter(here))))))
        +lsp)
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +brain
        +pomodoro
        +present
        +roam)           ; using org-mode for presentations
       plantuml          ; diagrams for confusing people more
       rest
       (ruby             ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +lsp
        +rails
        +rbenv)          ; rbenv as version manager
       web               ; the tubes

       :config
       ;;literate
       (default +bindings +smartparens))
