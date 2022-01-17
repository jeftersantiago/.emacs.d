(setq org-startup-folded t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(setq visual-fill-column-width 100 visual-fill-column-center-text t)

(setq-default fill-column 79)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq-default org-image-actual-width 620)
(setq org-latex-prefer-user-labels t)
(setq org-export-with-sub-superscripts nil)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(setq org-ellipsis "ᐯ")

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun efs/org-mode-visual-fill ()
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . efs/org-mode-visual-fill))

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
(add-to-list 'org-file-apps '("\\.pdf" . "xreader %s"))

;    (defvar org-export-output-directory-prefix "~/Documents" "prefix of directory used for org-mode export")

;    (defadvice org-export-output-file-name (before org-add-export-dir activate)
;      "Modifies org-export to place exported files in a different directory"
;      (when (not pub-dir)
;        (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
;        (when (not (file-directory-p pub-dir))
;          (make-directory pub-dir))))

(load "~/.emacs.d/external/ox-ipynb.el")
(require 'ox-ipynb)

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

;    (use-package org-ref
;      :disabled t
;      :config
;      (setq reftex-default-bibliography "~/bibliography2/references.bib")
;      (setq org-ref-default-bibliography "~/bibliography2/references.bib")
;      (setq org-ref-bibliography-notes "~/bibliography2/notes.org")
;      (setq org-ref-pdf-directory "~/bibliography2/pdfs")
;      (setq bibtex-completion-bibliography "~/bibliography2/references.bib")
;      (setq bibtex-completion-library-path "~/bibliography2/pdfs")
;      (setq bibtex-completion-notes-path "~/bibliography2/notes"))
