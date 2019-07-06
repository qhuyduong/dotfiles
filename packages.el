;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! add-node-modules-path)
(package! apib-mode)
(package! copy-as-format)
(package! dired-single)
(package! dockerfile-mode)
(package! docker-compose-mode)
(package! flycheck-apib :recipe (:fetcher github :repo "qhuyduong/flycheck-apib"))
(package! font-lock+ :recipe (:fetcher github :repo "emacsmirror/font-lock-plus"))
(package! google-translate)
(package! lorem-ipsum)
(package! mode-icons)
(package! nvm)
(package! ob-async)
(package! org-pivotal)
(package! org-pomodoro)
(package! osx-trash)
(package! persp-mode-projectile-bridge)
(package! prettier-js)
(package! reason-mode)
(package! uuidgen)

;;; Disabled packages
(package! eslintd-fix :disable t)
(package! evil-escape :disable t)
