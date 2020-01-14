;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! apib-mode)
(package! company-tern)
(package! copy-as-format)
(package! feature-mode)
(package! dired-single)
(package! docker-compose-mode)
(package! evil-matchit)
(package! evil-string-inflection)
(package! flycheck-apib :recipe (:host github :repo "qhuyduong/flycheck-apib"))
(package! google-translate)
(package! inflections)
(package! lorem-ipsum)
(package! multi-libvterm :recipe (:host github :repo "suonlight/multi-libvterm"))
(package! nvm)
(package! ob-async)
(package! org-pivotal)
(package! org-pomodoro)
(package! osx-trash)
(package! persp-mode-projectile-bridge)
(package! projectile-rails)
(package! react-snippets)
(package! reason-mode)
(package! tern)
(package! vimrc-mode)
(package! yaml-tomato)

;;; Disabled packages
(package! evil-escape :disable t)
(package! robe :disable t)
(package! tide :disable t)
