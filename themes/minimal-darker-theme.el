(deftheme minimal-darker
  "Minimal Darker color theme for Emacs 24 - Customized")

;; Please, install rainbow-mode.
;; Colors with +x are lighter. Colors with -x are darker.
(let ((minimal-darker-fg        "#e4e4ef")
      (minimal-darker-fg+1      "#f4f4ff")
      (minimal-darker-fg+2      "#f5f5f5")
      (minimal-darker-white     "#ffffff")
      (minimal-darker-black     "#000000")
      (minimal-darker-bg-1      "#101010")
      (minimal-darker-bg        "#181818")
      (minimal-darker-bg+1      "#282828")
      (minimal-darker-bg+2      "#453d41")
      (minimal-darker-bg+3      "#484848")
      (minimal-darker-bg+4      "#52494e")
      (minimal-darker-red-1     "#c73c3f")
      (minimal-darker-red       "#f43841")
      (minimal-darker-red+1     "#ff4f58")
      (minimal-darker-green     "#73c936")
      (minimal-darker-yellow    "#ffdd33")
      (minimal-darker-brown     "#cc8c3c")
      (minimal-darker-quartz    "#389D6C")
      (minimal-darker-niagara-2 "#303540")
      (minimal-darker-niagara-1 "#565f73")
      (minimal-darker-niagara   "#96a6c8")
      (minimal-darker-wisteria  "#9e95c7")
      (minimal-darker-gray      "#827676")
      (minimal-darker-blue      "#1F25C9")
      (minimal-darker-purple    "#E65E9B")
      )
  (custom-theme-set-variables 'minimal-darker
   '(frame-brackground-mode (quote dark)))

  (custom-theme-set-faces 'minimal-darker

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,minimal-darker-yellow))))
   `(font-lock-comment-face ((t (:foreground , minimal-darker-gray))))
   `(font-lock-comment-delimiter-face ((t (:foreground , minimal-darker-gray))))
   `(font-lock-doc-face ((t (:foreground , minimal-darker-gray))))

   `(font-lock-constant-face ((t (:foreground ,minimal-darker-quartz))))
   `(font-lock-doc-string-face ((t (:foreground ,minimal-darker-green))))

   `(font-lock-function-name-face ((t (:foreground ,minimal-darker-niagara))))
   `(font-lock-keyword-face ((t (:foreground ,minimal-darker-yellow :bold t))))

   `(font-lock-preprocessor-face ((t (:foreground ,minimal-darker-quartz))))
   `(font-lock-reference-face ((t (:foreground ,minimal-darker-quartz))))

   `(font-lock-string-face ((t (:foreground ,minimal-darker-green))))

   `(font-lock-type-face ((t (:foreground ,minimal-darker-quartz))))
   `(font-lock-variable-name-face ((t (:foreground ,minimal-darker-fg+1))))
   `(font-lock-warning-face ((t (:foreground ,minimal-darker-red))))


   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground , minimal-darker-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,minimal-darker-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,minimal-darker-purple))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground minimal-darker-niagara :bold t))))
   `(font-latex-slide-title-face ((t (:foreground ,minimal-darker-niagara))))
   `(font-latex-string-face ((t (:foreground ,minimal-darker-green))))
   `(font-latex-warning-face ((t (:foreground ,minimal-darker-red))))

   ;; Basic Coloring 
   `(border ((t ,(list :background minimal-darker-bg-1
                       :foreground minimal-darker-bg+2))))
   `(cursor ((t (:background ,minimal-darker-yellow))))
   `(default ((t ,(list :foreground minimal-darker-fg
                        :background minimal-darker-bg))))
   `(fringe ((t ,(list :background nil
                       :foreground minimal-darker-bg+2))))
   `(vertical-border ((t ,(list :foreground minimal-darker-bg+2))))
   `(link ((t (:foreground ,minimal-darker-niagara :underline t))))
   `(link-visited ((t (:foreground ,minimal-darker-wisteria :underline t))))
   `(match ((t (:background ,minimal-darker-bg+4))))
   `(shadow ((t (:foreground ,minimal-darker-bg+4))))

   `(minibuffer-prompt ((t (:foreground ,minimal-darker-niagara))))
   `(region ((t (:background ,minimal-darker-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background minimal-darker-bg+3
                                    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground minimal-darker-black
                                    :background minimal-darker-red))))
   `(tooltip ((t ,(list :background minimal-darker-bg+4
                        :foreground minimal-darker-white))))



   ;; Org Mode
   `(org-agenda-structure ((t (:foreground ,minimal-darker-niagara))))
   `(org-column ((t (:background ,minimal-darker-fg))))
   `(org-column-title ((t (:background ,minimal-darker-fg :underline t :weight bold))))
   `(org-done ((t (:foreground ,minimal-darker-green))))
   `(org-todo ((t (:foreground ,minimal-darker-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,minimal-darker-yellow))))


   ;; Agda2
   `(agda2-highlight-datatype-face ((t (:foreground ,minimal-darker-quartz))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,minimal-darker-quartz))))

   `(agda2-highlight-function-face ((t (:foreground ,minimal-darker-niagara))))
   `(agda2-highlight-keyword-face ((t ,(list :foreground minimal-darker-yellow
                                             :bold t))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,minimal-darker-green))))
   `(agda2-highlight-number-face ((t (:foreground ,minimal-darker-wisteria))))



   ;; Calendar
   `(holiday-face ((t (:foreground ,minimal-darker-red))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground minimal-darker-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground minimal-darker-brown
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,minimal-darker-red+1))))
   `(compilation-mode-line-fail ((t ,(list :foreground minimal-darker-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground minimal-darker-green
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; Completion
   `(completions-annotations ((t (:inherit 'shadow))))

   ;; Custom
   `(custom-state ((t (:foreground ,minimal-darker-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground minimal-darker-red+1
                             :background nil))))
   `(diff-added ((t ,(list :foreground minimal-darker-green
                           :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,minimal-darker-niagara :weight bold))))
   `(dired-ignored ((t ,(list :foreground minimal-darker-quartz
                              :inherit 'unspecified))))

   ;; Ebrowse
   `(ebrowse-root-class ((t (:foreground ,minimal-darker-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,minimal-darker-niagara))))

   ;; Egg
   `(egg-branch ((t (:foreground ,minimal-darker-yellow))))
   `(egg-branch-mono ((t (:foreground ,minimal-darker-yellow))))
   `(egg-diff-add ((t (:foreground ,minimal-darker-green))))
   `(egg-diff-del ((t (:foreground ,minimal-darker-red))))
   `(egg-diff-file-header ((t (:foreground ,minimal-darker-wisteria))))
   `(egg-help-header-1 ((t (:foreground ,minimal-darker-yellow))))
   `(egg-help-header-2 ((t (:foreground ,minimal-darker-niagara))))
   `(egg-log-HEAD-name ((t (:box (:color ,minimal-darker-fg)))))
   `(egg-reflog-mono ((t (:foreground ,minimal-darker-niagara-1))))
   `(egg-section-title ((t (:foreground ,minimal-darker-yellow))))
   `(egg-text-base ((t (:foreground ,minimal-darker-fg))))
   `(egg-term ((t (:foreground ,minimal-darker-yellow))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,minimal-darker-wisteria))))
   `(erc-timestamp-face ((t (:foreground ,minimal-darker-green))))
   `(erc-input-face ((t (:foreground ,minimal-darker-red+1))))
   `(erc-my-nick-face ((t (:foreground ,minimal-darker-red+1))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,minimal-darker-quartz))))
   `(eshell-ls-directory ((t (:foreground ,minimal-darker-niagara))))
   `(eshell-ls-executable ((t (:foreground ,minimal-darker-green))))
   `(eshell-ls-symlink ((t (:foreground ,minimal-darker-yellow))))
   ;; Flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,minimal-darker-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,minimal-darker-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,minimal-darker-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,minimal-darker-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,minimal-darker-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,minimal-darker-green :weight bold :underline t))))

   ;; Flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,minimal-darker-red) :inherit unspecified))
      (t (:foreground ,minimal-darker-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,minimal-darker-yellow) :inherit unspecified))
      (t (:foreground ,minimal-darker-yellow :weight bold :underline t))))

   ;; Helm
   `(helm-candidate-number ((t ,(list :background minimal-darker-bg+2
                                      :foreground minimal-darker-yellow
                                      :bold t))))
   `(helm-ff-directory ((t ,(list :foreground minimal-darker-niagara
                                  :background minimal-darker-bg
                                  :bold t))))
   `(helm-ff-executable ((t (:foreground ,minimal-darker-green))))
   `(helm-ff-file ((t (:foreground ,minimal-darker-fg :inherit unspecified))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground minimal-darker-bg
                                        :background minimal-darker-red))))
   `(helm-ff-symlink ((t (:foreground ,minimal-darker-yellow :bold t))))
   `(helm-selection-line ((t (:background ,minimal-darker-bg+1))))
   `(helm-selection ((t (:background ,minimal-darker-bg+1 :underline nil))))
   `(helm-source-header ((t ,(list :foreground minimal-darker-yellow
                                   :background minimal-darker-bg
                                   :box (list :line-width -1
                                              :style 'released-button)))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,minimal-darker-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,minimal-darker-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,minimal-darker-niagara :weight bold))))

   ;; Info
   `(info-xref ((t (:foreground ,minimal-darker-niagara))))
   `(info-visited ((t (:foreground ,minimal-darker-wisteria))))

   ;; Jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground minimal-darker-quartz
                                           :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,minimal-darker-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,minimal-darker-green))))
   `(jabber-rare-time-face ((t (:foreground ,minimal-darker-green))))
   `(jabber-roster-user-online ((t (:foreground ,minimal-darker-green))))
   `(jabber-activity-face ((t (:foreground ,minimal-darker-red))))
   `(jabber-activity-personal-face ((t (:foreground ,minimal-darker-yellow :bold t))))

   ;; Line Highlighting
   `(highlight ((t (:background ,minimal-darker-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background minimal-darker-bg+1
                                            :foreground nil))))

   ;; line numbers
   `(line-number ((t (:inherit default :foreground ,minimal-darker-bg+4))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,minimal-darker-yellow))))

   ;; Linum
   `(linum ((t `(list :foreground minimal-darker-quartz
                      :background minimal-darker-bg))))

   ;; Magit
   `(magit-branch ((t (:foreground ,minimal-darker-niagara))))
   `(magit-diff-hunk-header ((t (:background ,minimal-darker-bg+2))))
   `(magit-diff-file-header ((t (:background ,minimal-darker-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,minimal-darker-red+1))))
   `(magit-log-author ((t (:foreground ,minimal-darker-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground minimal-darker-green
                                            :background minimal-darker-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground minimal-darker-niagara
                                           :background minimal-darker-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground minimal-darker-yellow
                                          :background minimal-darker-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground minimal-darker-fg
                                          :background minimal-darker-bg+1))))
   `(magit-item-highlight ((t (:background ,minimal-darker-bg+1))))
   `(magit-tag ((t ,(list :foreground minimal-darker-yellow
                          :background minimal-darker-bg))))
   `(magit-blame-heading ((t ,(list :background minimal-darker-bg+1
                                    :foreground minimal-darker-fg))))

   ;; Message
   `(message-header-name ((t (:foreground ,minimal-darker-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background minimal-darker-bg+1
                          :foreground minimal-darker-white))))
   `(mode-line-buffer-id ((t ,(list :background minimal-darker-bg+1
                                    :foreground minimal-darker-white))))
   `(mode-line-inactive ((t ,(list :background minimal-darker-bg+1
                                   :foreground minimal-darker-quartz))))

   ;; Neo Dir
   `(neo-dir-link-face ((t (:foreground ,minimal-darker-niagara))))

  ;; Search
   `(isearch ((t ,(list :foreground minimal-darker-black
                        :background minimal-darker-fg+2))))
   `(isearch-fail ((t ,(list :foreground minimal-darker-black
                             :background minimal-darker-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground minimal-darker-fg+1
                                       :background minimal-darker-niagara-1))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,minimal-darker-red+1))))

   ;; Show Paren
   `(show-paren-match-face ((t (:background ,minimal-darker-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,minimal-darker-red-1))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,minimal-darker-red))))

   ;; Tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,minimal-darker-yellow))))

   ;; Speedbar
   `(speedbar-directory-face ((t ,(list :foreground minimal-darker-niagara
                                        :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,minimal-darker-fg))))
   `(speedbar-highlight-face ((t (:background ,minimal-darker-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,minimal-darker-red))))
   `(speedbar-tag-face ((t (:foreground ,minimal-darker-yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,minimal-darker-wisteria))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background minimal-darker-bg
                                 :foreground minimal-darker-bg+1))))
   `(whitespace-tab ((t ,(list :background minimal-darker-bg
                               :foreground minimal-darker-bg+1))))
   `(whitespace-hspace ((t ,(list :background minimal-darker-bg
                                  :foreground minimal-darker-bg+2))))
   `(whitespace-line ((t ,(list :background minimal-darker-bg+2
                                :foreground minimal-darker-red+1))))
   `(whitespace-newline ((t ,(list :background minimal-darker-bg
                                   :foreground minimal-darker-bg+2))))
   `(whitespace-trailing ((t ,(list :background minimal-darker-red
                                    :foreground minimal-darker-red))))
   `(whitespace-empty ((t ,(list :background minimal-darker-yellow
                                 :foreground minimal-darker-yellow))))
   `(whitespace-indentation ((t ,(list :background minimal-darker-yellow
                                       :foreground minimal-darker-red))))
   `(whitespace-space-after-tab ((t ,(list :background minimal-darker-yellow
                                           :foreground minimal-darker-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background minimal-darker-brown
                                            :foreground minimal-darker-brown))))

   ;; tab-bar
   `(tab-bar ((t (:background ,minimal-darker-bg+1 :foreground ,minimal-darker-bg+4))))
   `(tab-bar-tab ((t (:background nil :foreground ,minimal-darker-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil))))

   ;; vterm / ansi-term
   `(term-color-black ((t (:foreground ,minimal-darker-bg+3 :background ,minimal-darker-bg+4))))
   `(term-color-red ((t (:foreground ,minimal-darker-red-1 :background ,minimal-darker-red-1))))
   `(term-color-green ((t (:foreground ,minimal-darker-green :background ,minimal-darker-green))))
   `(term-color-blue ((t (:foreground ,minimal-darker-niagara :background ,minimal-darker-niagara))))
   `(term-color-yellow ((t (:foreground ,minimal-darker-yellow :background ,minimal-darker-yellow))))
   `(term-color-magenta ((t (:foreground ,minimal-darker-wisteria :background ,minimal-darker-wisteria))))
   `(term-color-cyan ((t (:foreground ,minimal-darker-quartz :background ,minimal-darker-quartz))))
   `(term-color-white ((t (:foreground ,minimal-darker-fg :background ,minimal-darker-white))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,minimal-darker-fg :background ,minimal-darker-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,minimal-darker-brown :background ,minimal-darker-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,minimal-darker-brown :background ,minimal-darker-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,minimal-darker-fg :background ,minimal-darker-bg-1))))
   `(company-tooltip-mouse ((t (:background ,minimal-darker-bg-1))))
   `(company-tooltip-common ((t (:foreground ,minimal-darker-green))))
   `(company-tooltip-common-selection ((t (:foreground ,minimal-darker-green))))
   `(company-scrollbar-fg ((t (:background ,minimal-darker-bg-1))))
   `(company-scrollbar-bg ((t (:background ,minimal-darker-bg+2))))
   `(company-preview ((t (:background ,minimal-darker-green))))
   `(company-preview-common ((t (:foreground ,minimal-darker-green :background ,minimal-darker-bg-1))))

   ;; Proof General
   `(proof-locked-face ((t (:background ,minimal-darker-niagara-2))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,minimal-darker-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,minimal-darker-green))))
   `(orderless-match-face-2 ((t (:foreground ,minimal-darker-brown))))
   `(orderless-match-face-3 ((t (:foreground ,minimal-darker-quartz))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'minimal-darker)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; minimal-darker-theme.el ends here.
