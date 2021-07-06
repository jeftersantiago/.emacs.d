(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

`use-package'
(unless(package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; puts the outputs in a separated file
(setq custom-file "~/.emacs.d/custom.el")

(org-babel-load-file (expand-file-name "~/.emacs.d/Emacs.org"))
;(set-frame-parameter nil 'fullscreen 'fullboth)
