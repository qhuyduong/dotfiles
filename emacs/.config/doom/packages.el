;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! apib-mode)
(package! copy-as-format)
(package! feature-mode)
(package! flycheck-apib :recipe (:host github :repo "qhuyduong/flycheck-apib"))
(package! evil-matchit)
(package! git-link)
(package! google-translate)
(package! lorem-ipsum)
(package! lsp-treemacs)
(package! multi-vterm)
(package! ob-mermaid)
(package! prettier-js)
(package! protobuf-mode)
(package! react-snippets)
(package! smali-mode :recipe (:host github :repo "strazzere/Emacs-Smali"))
(package! web-beautify)
(package! yaml-tomato)

;;; Disabled packages
(package! evil-escape :disable t)
(package! robe :disable t)
