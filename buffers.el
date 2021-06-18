
; Ace Window
; Controlling buffers
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 2.0)))))))

; Terminals
(use-package multi-term 
 :ensure t
 :config  (setq multi-term-program "/bin/bash")
 (progn
   (global-set-key (kbd "C-x t") 'multi-term)))
 
