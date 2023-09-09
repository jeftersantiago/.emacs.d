(setq frame-inhibit-implied-resize t)


(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

(require 'package)

; (setq package-enable-at-startup nil)
; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
; (package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))



; use-package
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless(package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; (defconst banner-title "E M A C S")

; (put 'dired-find-alternate-file 'disabled nil)
; (put 'set-goal-column 'disabled nil)
; (put 'narrow-to-region 'disabled nil)


(setq visible-bell "visual")

; (org-babel-load-file (expand-file-name "~/.emacs.d/newconfig.org"))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))





(use-package highlight-indentation
  :commands highlight-indentation-mode)








(use-package evil :ensure t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key websocket treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-evil smartparens simple-httpd rainbow-delimiters python-mode posframe pdf-tools no-littering magit-section lsp-ui lsp-treemacs julia-snail highlight-indentation gruber-darker-theme flycheck evil-collection elfeed elcord doom-themes doom-modeline-now-playing dired-sidebar dired-open default-text-scale dashboard counsel company auto-package-update auctex all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
