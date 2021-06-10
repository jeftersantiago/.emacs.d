;; Loading themes
;(use-package cherry-blossom-theme
;   :config(load-theme 'cherry-blossom t)
;  :ensure t)

(use-package dracula-theme
  :config(load-theme 'dracula t)
  :ensure t)

;; I like emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; Font
(add-to-list 'default-frame-alist '(font. "Monospace 13"))
;; took from here:  https://emacs.stackexchange.com/q/45895


(use-package default-text-scale
:ensure t
:hook (after-init . default-text-scale-mode))

(set-language-environment "UTF-8")
(global-prettify-symbols-mode t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq inhibit-startup-message t)

;(set-window-scroll-bars (minibuffer-window) nil nil)

; makes the parenthesis shine
(use-package rainbow-delimiters :ensure t)

(use-package mood-line :ensure t
:config (mood-line-mode))

; doesn't ask to confirm everytime
(setq confirm-kill-processes nil)

(setq-default truncate-lines t)
(setq-default fill-column 80)

;(setq-default cursor-type 'square)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Creates pairs of parenthesis in a smart way
(use-package smartparens
  :ensure t
  :config
  (sp-use-paredit-bindings)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
   kill-buffer-query-functions))
;; mouse scrolls very slowly
(setq scroll-step            1
scroll-conservatively  10000
mouse-wheel-scroll-amount '(1 ((shift) . 1))
mouse-wheel-progressive-speed nil
mouse-wheel-follow-mouse 't)

"Add a new line below the current line"
(defun insert-new-line-below ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "C-o") 'insert-new-line-below)
;; Disables syntax hilight
(global-set-key (kbd "C-x C-l") 'font-lock-mode)


(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1) 

