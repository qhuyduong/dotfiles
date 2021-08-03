;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       (company          ; the ultimate code completion backend
        +childframe)
       (ivy              ; a search engine for love and life
        +childframe
        +icons)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired            ; making dired pretty [functional]
        +icons)          ; colorful icons for dired-mode
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       (undo             ; persistent, smarter undo for your inevitable mistakes
        +tree)
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax           ; tasing you for every semicolon you forget
        +childframe)
       grammar           ; tasing grammar mistake every you make

       :tools
       (eval             ; run code, run (also, repls)
         +overlay)
       lookup            ; navigate your code and its documentation
       (lsp
        +peek)
       (magit            ; a git porcelain for Emacs
        +forge)
       tmux              ; an API for interacting with tmux

       :lang
       (cc               ; C > C++ == 1
        +lsp)
       emacs-lisp        ; drown in parentheses
       java              ; the poster child for carpal tunnel syndrome
       (javascript       ; all(hope(abandon(ye(who(enter(here))))))
        +lsp)
       json              ; At least it ain't XML
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +roam)
       rest
       (ruby             ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +lsp)
       web               ; the tubes
       yaml              ; JSON, but readable

       :config
       ;;literate
       (default +bindings +smartparens))
