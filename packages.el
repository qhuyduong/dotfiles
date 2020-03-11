;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! apib-mode)
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
(package! monkeyc-mode :recipe (:host github :repo "4lick/monkeyc-mode"))
(package! nvm)
(package! org-pivotal)
(package! osx-trash)
(package! persp-mode-projectile-bridge)
(package! prettier-js)
(package! projectile-rails)
(package! react-snippets)
(package! reason-mode)
(package! vimrc-mode)
(package! yaml-tomato)

;;; Disabled packages
(package! evil-escape :disable t)
(package! robe :disable t)
