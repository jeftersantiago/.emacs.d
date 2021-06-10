(setq-default user-full-name "Jefter Santiago")
(setq-default user-mail-address "jeftersantiago@protonmail.com")

(load "~/.local/bin/private.el")


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa"."https://melpa.org/packages/"))
(package-initialize)


`use-package'
(unless(package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; puts the outputs in a separated file 
(setq custom-file "~/.emacs.d/custom.el")

(load "~/.emacs.d/env.el")
(load "~/.emacs.d/programming.el")
(load "~/.emacs.d/external.el")
(load "~/.emacs.d/dired.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/buffers.el")
(load "~/.emacs.d/latex.el")


