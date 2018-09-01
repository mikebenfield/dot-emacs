
(provide 'my-org)

(defun my-org-hook ()
  ;; for some bizarre reason my org mode doesn't have this binding...
  ;; and this hook isn't helping. ???
  (setq paragraph-start "\\|[    ]*$"
        paragraph-separate "[     ]*$"))

(add-hook 'org-mode-hook #'my-org-hook)

;; no blurry LaTeX previews
(setq org-latex-create-formula-image-program 'dvisvgm)
;; (setq org-latex-create-formula-image-program 'dvipng)
