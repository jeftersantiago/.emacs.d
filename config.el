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
(global-font-lock-mode 0)

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq confirm-kill-processes nil)
(setq-default truncate-lines t)
(setq-default fill-column 80)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default auto-fill-function 'do-auto-fill)

(setq-default cursor-type 'square)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq frame-resize-pixelwise t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(use-package doom-themes
      :ensure t
      :init (load-theme 'doom-dracula t))
;   (use-package spacemacs-theme
;     :defer t
;     :init (load-theme 'spacemacs-dark t))

(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 99 99))

(when (member "Iosevka" (font-family-list))
  (progn
    (set-frame-font "Iosevka-10" nil t)))


(use-package default-text-scale
  :ensure t
  :hook (after-init . default-text-scale-mode))
(set-language-environment "UTF-8")
(global-prettify-symbols-mode t)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-x C-k") 'font-lock-mode)

(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25))
  :ensure t)

(setq display-line-numbers-type 'relative)

(global-set-key (kbd "C-x C-l") 'global-display-line-numbers-mode)

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-center-content nil)
    (setq dashboard-banner-logo-title "EMACS")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-startup-banner "~/.emacs.d/images/emacs-logo.png")
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)
                            (agenda . 0)
                            (bookmarks . 0)
                            (registers . 0)))
    )
  :config
  (dashboard-setup-startup-hook))

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

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t)
(evil-mode 1)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

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

(use-package dired-sidebar
  :after dired
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init)

(use-package all-the-icons-dired
  :after dired
  :ensure t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired-open
      :after dired
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
;            ("pdf" . "evince")
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

(setq org-startup-folded t)
      (setq org-src-tab-acts-natively t)
      (setq org-src-window-setup 'current-window)
      (setq org-src-fontify-natively t)
      (setq org-hide-emphasis-markers t)
;      (setq modus-themes-intense-markup t)

      (setq visual-fill-column-width 100 visual-fill-column-center-text t)

      (setq-default fill-column 79)
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps nil)

      (setq-default org-image-actual-width 620)
      (setq org-latex-prefer-user-labels t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(setq org-ellipsis "ᐯ")

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Inconsolata" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook 'font-lock-mode)
(add-hook 'org-mode-hook 'hl-line-mode)

(defun efs/org-mode-visual-fill ()
   (visual-fill-column-mode 2))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-emphasis-alist
           '(("*" bold)
             ("/" italic)
             ("_" underline)
             ("=" org-verbatim verbatim)
             ("~" org-code verbatim)
             ("+" (:strick-through t))))

(defface custom-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f21782"  :weight ultra-bold))
  "My bold emphasis for Org.")

     (defface custom-italic
       '((default :inherit italic)
         (((class color) (min-colors 88) (background light))
          :foreground "#005e00")
         (((class color) (min-colors 88) (background dark))
          :foreground "#f21782"))
       "Italic emphasis for Org.")

     (defface custom-underline
       '((default :inherit underline)
         (((class color) (min-colors 88) (background light))
          :foreground "#813e00")
         (((class color) (min-colors 88) (background dark))
          :foreground  "#d36198" ))
       "Underline emphasis for Org.")

     (defface custom-strike-through
       '((((class color) (min-colors 88) (background light))
          :strike-through "#BABDB6" :foreground "#FF0000")
         (((class color) (min-colors 88) (background dark))
          :strike-through "#d36198" :foreground "#ff0023"))
       "Custom strike-through for Org.")

     (setq org-emphasis-alist
           '(("*" custom-bold)
             ("/" custom-italic)
             ("_" custom-underline)
             ("=" org-verbatim fixed-pitch)
             ("~" org-code fixed-pitch)
             ("+" (bold custom-strike-through))))

(custom-set-faces
     '(org-document-title ((t(
                              :weight ultra-bold 
                              :height 1.2
                              :foreground "#f21782"
;                              :box (:line-width 1 :color "#f21782")
                              ))))
     '(org-document-info ((t(
                              :weight bold
                              :height 1.2
                              :foreground "#d36198"
                              ))))
     )

(use-package imenu
  :ensure t
  :after org-mode)
(setq org-imenu-depth 3)

(use-package imenu-list
  :ensure t
  :after org-mode)

(setq  imenu-list-position 'left
       imenu-list-size 55
       imenu-list-focus-after-activation t)

(global-set-key (kbd "C-l") #'imenu-list-minor-mode)
(setq imenu-list-focus-after-activation nil)


; (add-hook 'after-save-hook 'imenu-list-refresh)

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

(setq org-image-actual-width 300)
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

(setq org-export-with-sub-superscripts nil)
(add-hook 'org-mode-hook
          (lambda () (org-toggle-pretty-entities)))
;; Opening pdfs
(add-to-list 'org-file-apps '("\\.pdf" .  "evince %s"))

;    (defvar org-export-output-directory-prefix "~/Documents" "prefix of directory used for org-mode export")

;    (defadvice org-export-output-file-name (before org-add-export-dir activate)
;      "Modifies org-export to place exported files in a different directory"
;      (when (not pub-dir)
;        (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
;        (when (not (file-directory-p pub-dir))
;          (make-directory pub-dir))))

(global-set-key (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)
(global-set-key (kbd "C-c i") 'org-toggle-inline-images)
(global-set-key (kbd "C-x p") 'org-latex-export-to-pdf)
(define-key global-map "\C-cc" 'org-capture)

; load the latex fragments automatically
(use-package org-fragtog :ensure t)
(add-hook 'org-mode-hook 'org-fragtog-mode)

                                        ; using dvipng makes it faster, but with less quality
(setq org-latex-create-formula-image-program  'dvisvgm)

                                        ; adjusting the size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

                                        ;     (setq org-latex-caption-above nil)

(use-package cdlatex
  :ensure t)
(add-hook 'cdlatex-mode-hook
          (lambda () (when (eq major-mode 'org-mode)
                       (make-local-variable 'org-pretty-entities-include-sub-superscripts)
                       (setq org-pretty-entities-include-sub-superscripts nil))))
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(setq org-latex-to-pdf-process (list "latexmk -pvc -pdf %f"))

(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("frame" "") ("linenos=true")))

;      (add-hook 'org-mode-hook
                                        ;        (lambda () (texfrag-mode))

(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))



(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (fortran . t)
     (C . t)
     (gnuplot . t)
     ))
  (setq org-confirm-babel-evaluate t))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo t)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("jl" . "src julia"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (file-truename "~/Dropbox/notes/"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "Default notes" plain
      "%?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "Notes on physics" plain
      "#+setupfile:~/Dropbox/Templates/physics.org \n* %?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "Notes on mathematics" plain
      "#+setupfile:~/Dropbox/Templates/mathematics.org \n* %?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "Notes on computing" plain
      "#+setupfile:~/Dropbox/Templates/computing.org \n* %?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("s" "Paper" plain
      "#+setupfile:~/Dropbox/Templates/paper.org \n* %?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      )
     ("r" "bibliography reference" plain "%?"
      :target
      (file+head "~/Dropbox/references/${citekey}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package websocket
  :after org-roam
  :ensure t)

(use-package org-roam-ui
  :after org-roam
  :ensure t)

(use-package helm-bibtex
  :ensure t)
(setq bibtex-completion-bibliography
      '("~/Dropbox/references/ic.bib"
        "~/Dropbox/references/lab-lib.bib"
        "~/Dropbox/references/cs.bib"))

(setq bibtex-completion-pdf-field "file")

(setq bibtex-completion-notes-path "~/Dropbox/notes/")

(setq bibtex-completion-browser-function
      (lambda (url _) (start-process "firefox" "*firefox*" "firefox" url)))

(use-package org-roam-bibtex
:after org-roam
:load-path "~/.emacs.d/external/org-roam-bibtex/" ; Modify with your own path where you cloned the repository
:config
(require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

(setq org-noter-set-start-location "~/Dropbox/Papers/")

;    (use-package pdf-tools
;      :ensure t
;      :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;      :magic ("%PDF" . pdf-view-mode)
;      :config
;      (pdf-tools-install))
    ;(global-set-key (kbd "C-c i") 'pdf-view-midnight-minor-mode)

(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install :no-query)
  :magic ("%PDF" . pdf-view-mode))

(defun pdf-open-evince ()
  "Opens the PDF with ´xreader´."
  (interactive)
  (save-window-excursion
    (let ((current-file (buffer-file-name))
          (current-page (number-to-string (pdf-view-current-page))))
      (async-shell-command
       (format "xreader -i %s \"%s\"" current-page current-file))))
  (message "Sent to xreader"))

(defun pdf-open-xournalpp ()
  "Opens the PDF with ´xreader´."
  (interactive)
  (save-window-excursion
    (let ((current-file (buffer-file-name))
          (current-page (number-to-string (pdf-view-current-page))))
      (async-shell-command
       (format "GTK_THEME=Materia-light-compact: xournalpp \"%s\"" current-page current-file))))
  (message "Sent to Xournal++"))

(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
(define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
                                        ;    (define-key pdf-view-mode-map (kbd "m") 'pdf-view-midnight-minor-mode)

(define-key pdf-view-mode-map [(return)] 'pdf-open-evince)
(define-key pdf-view-mode-map [(shift return)] 'pdf-open-xournalpp)

(define-key pdf-view-mode-map (kbd "P") 'pdf-view-printer-minor-mode)
(define-key pdf-view-mode-map (kbd "M") 'pdf-view-set-slice-using-mouse)
(define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window)
(define-key pdf-view-mode-map (kbd "f") 'pdf-view-fit-height-to-window)

                                         ;    (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(use-package auctex
       :hook ((latex-mode LaTeX-mode) . lsp)
       :ensure t
       :config
       (add-to-list 'texmathp-tex-commands "dmath" 'env-on)
       (texmathp-compile)
       :init
       (setq-default TeX-master 'shared)
       ;; nil is the default; this remains here as a reminder that setting it to
       ;; true makes emacs hang on every save when enabled.
       (setq TeX-auto-save nil)
       (setq TeX-parse-self t))

     (setq-default TeX-master nil)
;     (use-package auctex-latexmk
;       :config
;       (setq auctex-latexmk-inherit-TeX-PDF-mode t)
;       :init
;       (auctex-latexmk-setup))

     (add-hook 'LaTeX-mode-hook 'visual-line-mode)
     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")))

(setq TeX-view-program-list
      '(("PDF Viewer" "xreader %o")))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("PdfLatex" "pdflatex -interaction=nonstopmode %s" TeX-run-command t t :help "Run pdflatex") t))

(add-hook 'cdlatex-mode-hook
          (lambda () (when (eq major-mode 'org-mode)
                       (make-local-variable 'org-pretty-entities-include-sub-superscripts)
                       (setq org-pretty-entities-include-sub-superscripts nil))))

(add-hook 'org-mode-hook
          (lambda () (org-toggle-pretty-entities)))

;   (defun set-black-face ()
;     (set-face-background 'default "#000000"))


    (use-package vterm
      :ensure t
      :config 
      (global-set-key (kbd "C-x t") 'vterm))
    (add-hook 'vterm-mode-hook 'font-lock-mode)
;   (add-hook 'vterm-mode-hook 'set-black-face)

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
  :defer 0
  :ensure t
  :config (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/Projects/yasnippets"
                           "~/Dropbox/private-configs/private-snippets/"))
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/")
    (setq projectile-project-search-path '("~/Projects/"))))
(setq projectile-switch-projects-action #'projectile-dired)
(use-package counsel-projectile
  :after projectile
  :ensure t
  :config (counsel-projectile-mode))

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

(use-package simple-httpd
  :commands http-server-directory
  :ensure t)

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

;; Now run `M-x vterm` and make sure it works!

(use-package julia-snail
  :ensure t
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

(use-package gnuplot-mode
  :ensure t)
(add-to-list 'load-path "~/.emacs.d/gnuplot/gnuplot-mode.el")
(autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(require 'ob-gnuplot)
(use-package gnuplot :ensure t)

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
   (setq company-minimum-prefix-length 2)
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

  (global-set-key (kbd "C-c d") 'elcord-mode)

  (setq elcord-use-major-mode-as-main-icon t)
  (setq elcord-display-buffer-detail 'nil)
  (setq elcord-refresh-rate 2)
  :init)

(setq gc-cons-threshold (* 2 1000 1000))
