(use-package auctex
  :ensure t
  :hook ((latex-mode LaTeX-mode) . tex)
  :config
  (font-lock-mode)
  (add-to-list 'font-latex-math-environments "dmath"))

(add-hook 'LaTeX-mode-hook 'TeX-mode)
(add-hook 'LaTeX-mode-hook 'font-lock-mode)


(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-AUCTeX t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
                                        ; (add-hook 'LateX-mode-hook (lambda () (latex-preview-pane-mode)))
                                        ; (global-set-key (kbd "C-x l ") 'latex-preview-pane-mode)
(global-set-key (kbd "C-x l ") 'pdflatex)
(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)   
(setq org-latex-listings-options '(("breaklines" "true")))
