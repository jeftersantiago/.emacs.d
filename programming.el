;; Ivy
(use-package ivy
  :ensure t
  :config(ivy-mode 1))
;; Swiper
(use-package swiper
  :ensure t
  :config
  (progn
	(ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)))


; Try
(use-package try
  :ensure t
  :config
  (progn  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)))

(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy)
; Which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))


;; backup disabled
(setq-default backup-inhibited t)
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(use-package real-auto-save
  :ensure t
  :demand t
  :config (setq real-auto-save-interval 10)
  :hook (prog-mode . real-auto-save-mode))

