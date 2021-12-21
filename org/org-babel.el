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
