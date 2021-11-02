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
(fringe-mode 0)
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

(setq frame-resize-pixelwise t)

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)
    :ensure t)
;  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(use-package doom-themes
          :init (load-theme 'doom-Iosvkem t))

;        (use-package spacemacs-theme
;          :defer t
;          :init (load-theme 'spacemacs-dark t))
  ;(set-background-color "black")

(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))

(set-frame-font "Monospace-12:antialias=true")

(use-package default-text-scale
  :ensure t
  :hook (after-init . default-text-scale-mode))
(set-language-environment "UTF-8")
(global-prettify-symbols-mode t)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-x C-l") 'font-lock-mode)

(use-package all-the-icons :ensure t)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25))
  :ensure t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

; (use-package dired-sidebar
;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;   :ensure t
;   :commands (dired-sidebar-toggle-sidebar)
;   :init)

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

(defun insert-new-line-below ()
     (interactive)
     (let ((oldpos (point)))
       (end-of-line)
       (newline-and-indent)))

(global-set-key (kbd "C-o") 'insert-new-line-below)

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

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(use-package smartparens
  :ensure t
  :config
  (sp-use-paredit-bindings)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))



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

(setq org-startup-folded t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(setq visual-fill-column-width 100 visual-fill-column-center-text t)

(setq-default fill-column 79)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq-default org-image-actual-width 620)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(setq org-ellipsis "ᐯ")

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

; (defun efs/org-mode-visual-fill ()
;   (visual-fill-column-mode 1))

; (use-package visual-fill-column
;   :ensure t
;   :hook (org-mode . efs/org-mode-visual-fill))

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

(setq org-latex-pdf-process (list
   "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

(add-hook 'org-mode-hook
  (lambda () (org-toggle-pretty-entities)))
;; Opening pdfs
(add-to-list 'org-file-apps '("\\.pdf" . "xreader %s"))

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

;      (add-hook 'org-mode-hook
;        (lambda () (texfrag-mode))

      (add-to-list 'org-latex-packages-alist
                   '("" "tikz" t))
      (eval-after-load "preview"
        '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(use-package org-ref
  :disabled t
  :config
  (setq reftex-default-bibliography "~/bibliography2/references.bib")
  (setq org-ref-default-bibliography "~/bibliography2/references.bib")
  (setq org-ref-bibliography-notes "~/bibliography2/notes.org")
  (setq org-ref-pdf-directory "~/bibliography2/pdfs")
  (setq bibtex-completion-bibliography "~/bibliography2/references.bib")
  (setq bibtex-completion-library-path "~/bibliography2/pdfs")
  (setq bibtex-completion-notes-path "~/bibliography2/notes"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ))
(setq org-confirm-babel-evaluate t)

(require 'org-tempo)
(add-to-list 'org-modules 'org-tempo t)

(with-eval-after-load 'org
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python")))

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

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package auctex
  :ensure t
  :hook ((latex-mode LaTeX-mode) . tex)
  :config
  (add-to-list 'font-latex-math-environments "dmath"))
(add-hook 'LaTeX-mode-hook 'TeX-mode)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq TeX-auto-save nil)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")))

(setq TeX-view-program-list
      '(("PDF Viewer" "xreader %o")))

(eval-after-load "tex"
            '(add-to-list 'TeX-command-list
                          '("PdfLatex" "pdflatex -interaction=nonstopmode %s" TeX-run-command t t :help "Run pdflatex") t))

(add-to-list 'load-path "~/.emacs.d/lsp/lsp-latex.el")
     (require 'lsp-latex)
     ;; "texlab" must be located at a directory contained in `exec-path'.
     ;; If you want to put "texlab" somewhere else,
     ;; you can specify the path to "texlab" as follows:
     ;; (setq lsp-latex-texlab-executable "/path/to/texlab")

     (with-eval-after-load "tex-mode"
      (add-hook 'tex-mode-hook 'lsp)
      (add-hook 'latex-mode-hook 'lsp))


     ;; For bibtex
;    (with-eval-after-load "bibtex"
;     (add-hook 'bibtex-mode-hook 'lsp))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))


(use-package lsp-ivy
  :ensure t
  :after lsp)

(use-package lsp-treemacs
  :ensure t
  :after lsp)


(use-package lsp-mode
  :commands lsp
  :hook ((fortran-mode f90-mode sh-mode) . lsp)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-snippet nil)
  (setq lsp-file-watch-threshold 500000)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-rust-clippy-preference "on"))

(use-package eglot
  :ensure t)
(add-hook 'LaTeX-mode-hook 'eglot-ensure)

;; function decides whether .h file is C or C++ header, sets C++ by
;; default because there's more chance of there being a .h without a
;; .cc than a .h without a .c (ie. for C++ template files)
(defun ejb/c-c++-header ()
  "Sets either c-mode or c++-mode, whichever is appropriate for
the header, based upon the associated source code file."
  (interactive)
  (let ((c-filename (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-filename)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . ejb/c-c++-header))

(defun ejb/c-c++-toggle ()
  "Toggles a buffer between c-mode and c++-mode."
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

(setq c-basic-offset 4)
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r")))
(setq c-doc-comment-style
      '((c-mode . javadoc)
        (java-mode . javadoc)
        (pike-mode . autodoc)))

(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-mode" my-cc-style)

(use-package ccls
  :ensure t
  :after lsp-mode
  :hook ((c-mode c++-mode) . lsp))

(use-package clang-format
  :ensure t
  :bind (("C-M-<tab>" . clang-format-region)))

(use-package astyle
  :ensure t
  :when (executable-find "astyle"))

(use-package julia-mode :ensure t)
;; Snail requires vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-always-compile-module t))

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode))

(use-package lsp-julia
  :hook (julia-mode . (lambda ()
                        (require 'lsp-julia)
                        (lsp)))
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6"))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

(use-package company
   :ensure t
   :config
   (setq company-idle-delay 0)
   (setq company-minimum-prefix-length 1)
   :init (global-company-mode t))

(use-package company-box
  :ensure t
  :hook (global-company-mode . company-box))


 (use-package company-irony
   :ensure t
   :config
   (add-to-list 'company-backends 'company-irony))

 (use-package irony
   :ensure t
   :config
   (add-hook 'c++-mode-hook 'irony-mode)
   (add-hook 'c-mode-hook 'irony-mode)
   (add-hook 'irony-mode-hook 'irony-cdb-auto-setup-compile-options))

 (use-package irony-eldoc
   :ensure t
   :config
   (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package elcord
    :ensure t
    :config
    (setq elcord-use-major-mode-as-main-icon t)
    (setq elcord-display-buffer-detail 'nil)
    (setq elcord-refresh-rate 2)
    :init)

(global-set-key (kbd "C-c d") 'elcord-mode)
