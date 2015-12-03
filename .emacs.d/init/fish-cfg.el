;;; fish-cfg.el --- fish config

;;; Commentary:
;;; Code:

(defun fish-hook ()
  (setq-local company-backends '((company-shell company-dabbrev-code company-files company-yasnippet))))

(add-hook 'fish-mode-hook #'fish-hook)

(with-eval-after-load "fish-mode"

  (defun fish-snippet (snippet)
    (evil-insert-state)
    (-> snippet (yas-lookup-snippet) (yas-expand-snippet)))

  (modify-syntax-entry ?~ "_" fish-mode-syntax-table)
  (modify-syntax-entry ?. "_" fish-mode-syntax-table)

  (define-key fish-mode-map (kbd "C-c C-c") #'executable-interpret)
  (define-key fish-mode-map (kbd "C-M-x")   #'sh-execute-region)

  (evil-leader/set-key-for-mode 'fish-mode
    "<tab> I" #'fish_indent
    "<tab> i" (lambda () (interactive) (fish-snippet "if"))
    "<tab> f" (lambda () (interactive) (fish-snippet "func"))
    "<tab> o" (lambda () (interactive) (fish-snippet "for"))))

(provide 'fish-cfg)
;;; dired-cfg.el ends here
