;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq-default user-full-name "Jefter Santiago")
(setq-default user-mail-address "jeftersantiago@protonmail.com")
(load "~/Dropbox/private-configs/private.el")

(setq custom-file "~/.emacs.d/custom.el")

(setq-default backup-inhibited t)
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(use-package real-auto-save
  :ensure t
  :demand t
  :config (setq real-auto-save-interval 10)
  :hook (prog-mode . real-auto-save-mode))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package no-littering :ensure t )

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(fringe-mode 2)
(font-lock-mode t)

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq confirm-kill-processes nil)
(setq-default truncate-lines t)
(setq-default fill-column 80)

(setq-default cursor-type 'square)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package rainbow-delimiters :ensure t)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

; (use-package doom-themes
;   :config
;   (load-theme 'doom-Iosvkem t)
;   :ensure t)
  (use-package modus-themes 
    :config
   (load-theme 'modus-operandi t)
    :ensure t)

(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 99 99))

(set-frame-font "Roboto-12:antialias=true")

(use-package default-text-scale
  :ensure t
  :hook (after-init . default-text-scale-mode))
(set-language-environment "UTF-8")
(global-prettify-symbols-mode t)

(prefer-coding-system 'utf-8)

(use-package all-the-icons :ensure t)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))
  :ensure t)

; (global-display-line-numbers-mode)
; (setq display-line-numbers-type 'relative)

(use-package dired-sidebar
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'font-lock-mode))

(use-package all-the-icons-dired :ensure t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
        '(("doc" . "openoffice4")
          ("docx" . "openoffice4")
          ("xopp" . "xournalpp")
          ("gif" . "mirage")
          ("jpeg" ."mirage")
          ("jpg" . "mirage")
          ("png" . "mirage")
          ("mkv" . "mpv")
          ("avi" . "mpv")
          ("mov" . "mpv")
          ("mp3" . "mpv")
          ("mp4" . "mpv")
          ("pdf" . "xreader")
          ("webm" . "mpv"))))

(use-package dired-hide-dotfiles
  :ensure t
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(setq-default dired-listing-switches "-lhvA")
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
;; Taken from here: https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables/13096#13096
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactivye)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(use-package smartparens
  :ensure t
  :config
  (sp-use-paredit-bindings)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))



(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
;; mouse scrolls very slowly
(setq confirm-kill-processes nil)
(setq scroll-step            1
      scroll-conservatively  10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(defun insert-new-line-below ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(use-package ace-window
  :ensure t
  :init
  (progn
   (global-set-key [remap other-window] 'ace-window)
   (custom-set-faces
    '(aw-leading-char-face
      ((t (:inherit ace-jump-face-foreground :height 2.0)))))))

(use-package multi-term :ensure t)
(setq multi-term-program "/bin/bash")

(global-set-key (kbd "C-x t") 'multi-term)
(global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-x C-l") 'font-lock-mode)
(global-set-key (kbd "C-c d") 'elcord-mode)

(global-set-key (kbd "C-o") 'insert-new-line-below)

(use-package ivy
  :ensure t
  :config(ivy-mode 1))

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)))

(use-package try
  :ensure t
  :config
  (progn  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)))
(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(setq org-startup-folded t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(setq visual-fill-column-width 100 visual-fill-column-center-text t)

(setq-default fill-column 79)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq-default org-image-actual-width 620)

(require 'org-tempo)
(add-to-list 'org-modules 'org-tempo t)

(with-eval-after-load 'org
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(setq org-ellipsis "ᐯ")

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;  (defun efs/org-mode-visual-fill ()
;    (visual-fill-column-mode 1))

;  (use-package visual-fill-column
;    :ensure t
;    :hook (org-mode . efs/org-mode-visual-fill))

(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DROP(x!)"))
      org-log-into-drawer t)

(defun org-file-path (filename)
  ;; return the absolute address of an org file, give its relative name
  (concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "TODOs.org"))
(setq org-archive-location
      (concat (org-file-path "DONE.org") "::* From %s"))

;; copy the content out of the archive.org file and yank in the inbox.org
(setq org-agenda-files (list org-index-file))
;; mark  a todo as done and move it to an appropriate place in the archive.
(defun hrs/mark-done-and-archive ()
  ;; mark the state of an org-mode item as DONE and archive it.
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))
(setq org-log-done 'time)

(setq org-capture-templates
      '(("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n")))

(defun my-org-capture-place-template-dont-delete-windows (oldfun args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(with-eval-after-load "org-capture"
  (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)
(add-hook 'org-mode-hook 'org-toggle-inline-images)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(add-hook 'org-mode-hook
  (lambda () (org-toggle-pretty-entities)))
;; Opening pdfs
;;(add-to-list 'org-file-apps '("\\.pdf" . "xreader %s"))

(global-set-key (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)
(global-set-key (kbd "C-c i") 'org-toggle-inline-images)
(global-set-key (kbd "C-x p") 'org-latex-export-to-pdf)
(define-key global-map "\C-cc" 'org-capture)

; load the fragments automatically
(use-package org-fragtog :ensure t)
(add-hook 'org-mode-hook 'org-fragtog-mode)

; this is the only way to really work (idk y)
(setq org-latex-create-formula-image-program 'dvisvgm)

; adjusting the size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(use-package auctex
  :ensure t
  :hook ((latex-mode LaTeX-mode) . tex)
  :config
  (add-to-list 'font-latex-math-environments "dmath"))
(add-hook 'LaTeX-mode-hook 'TeX-mode)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;  (setq latex-run-command "pdflatex -interaction=nonstopmode")

  (setq TeX-view-program-selection
        '((output-pdf "PDF Viewer")))

  (setq TeX-view-program-list
        '(("PDF Viewer" "xreader %o")))

  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("PdfLatex" "pdflatex -interaction=nonstopmode %s" TeX-run-command t t :help "Run pdflatex") t))

(use-package auto-complete
  :ensure t
  :init
  (auto-complete-mode))
(auto-complete-mode 1)

(use-package elcord
  :ensure t
  :config
  (setq elcord-use-major-mode-as-main-icon t)
  (setq elcord-display-buffer-detail 'nil)
  (setq elcord-refresh-rate 2)
  :init)
