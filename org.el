; this allows to use some shortcuts .. begins_src..
(require 'org-tempo)

(add-to-list 'org-modules 'org-tempo t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq org-ellipsis "⮟")

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

; (setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist
			 '("el" . "src emacs-lisp"))


; Tasks management 
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq-default fill-column 79)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DROP(x!)"))
      org-log-into-drawer t)

(defun org-file-path (filename)
  " Return the absolute address of an org file, give its relative name"
  (concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "todo.org"))
(setq org-archive-location
      (concat (org-file-path "done.org") "::* From %s"))

;; copy the content out of the archive.org file and yank in the inbox.org
(setq org-agenda-files (list org-index-file))
                                        ; mark  a todo as done and move it to an appropriate place in the archive.
(defun hrs/mark-done-and-archive ()
  " Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))
(global-set-key (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)
(setq org-log-done 'time)


; Capturing Tasks
(setq org-capture-templates
      '(("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n")))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(define-key global-map "\C-cc" 'org-capture)

; Displaying inline images
; The joy of programming = https://joy.pm/post/2017-09-17-a_graphviz_primer/nn

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)
(setq-default org-image-actual-width 620)
(global-set-key (kbd "C-c i") 'org-toggle-inline-images)

; Exporting with org-mode

; Makes UTF-8 symbols appears in buffer I use it for editing Latex 

(add-hook 'org-mode-hook
(lambda () (org-toggle-pretty-entities))) 

;; Opening pdfs
(add-to-list 'org-file-apps '("\\.pdf" . "xreader %s"))
(global-set-key (kbd "C-x p") 'org-latex-export-to-pdf)





