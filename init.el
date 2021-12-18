(setq frame-inhibit-implied-resize t)
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


(require 'use-package)
(setq use-package-always-ensure t)

(defconst banner-title "E M A C S")



(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
