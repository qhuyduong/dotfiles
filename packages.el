;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! add-node-modules-path)
(package! apib-mode)
(package! copy-as-format)
(package! feature-mode)
(package! dired-single)
(package! dockerfile-mode)
(package! docker-compose-mode)
(package! evil-matchit)
(package! evil-string-inflection)
(package! flycheck-apib :recipe (:host github :repo "qhuyduong/flycheck-apib"))
(package! google-translate)
(package! import-js :recipe (:host github :repo "qhuyduong/emacs-import-js"))
(package! inflections)
(package! lorem-ipsum)
(package! nvm)
(package! ob-async)
(package! org-pivotal)
(package! org-pomodoro)
(package! osx-trash)
(package! persp-mode-projectile-bridge)
(package! prettier-js)
(package! projectile-rails)
(package! reason-mode)

;;; Disabled packages
(package! eslintd-fix :disable t)
(package! evil-escape :disable t)
