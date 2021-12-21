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
