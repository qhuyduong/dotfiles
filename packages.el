;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! add-node-modules-path)
(package! apib-mode)
(package! copy-as-format)
(package! dired-single)
(package! dockerfile-mode)
(package! docker-compose-mode)
(package! evil-matchit)
(package! evil-terminal-cursor-changer)
(package! flycheck-apib :recipe (:host github :repo "qhuyduong/flycheck-apib"))
(package! google-translate)
(package! icons-in-terminal :recipe (:host github :repo "seagle0128/icons-in-terminal.el"))
(package! inflections)
(package! lorem-ipsum)
(package! navigate :recipe (:host github :repo "keith/evil-tmux-navigator"))
(package! nvm)
(package! ob-async)
(package! org-pivotal)
(package! org-pomodoro)
(package! osx-trash)
(package! persp-mode-projectile-bridge)
(package! prettier-js)
(package! projectile-rails)
(package! reason-mode)
(package! uuidgen)

;;; Disabled packages
(package! eslintd-fix :disable t)
(package! evil-escape :disable t)
