(("src"
  . ((emacs-lisp-mode
      . ((eval . (when (fboundp (quote flycheck-mode)) (flycheck-mode 1)))
         (eval . (when (fboundp (quote flycheck-package-setup)) (flycheck-package-setup))))))))
