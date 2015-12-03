;;; fish-cfg.el --- fish config

;;; Commentary:
;;; Code:

(defun fish-hook ()
  (setq-local company-backends '((company-shell company-dabbrev-code company-files company-yasnippet))))

(add-hook 'fish-mode-hook #'fish-hook)

(with-eval-after-load "fish-mode"

  (modify-syntax-entry ?~ "_" fish-mode-syntax-table)
  (modify-syntax-entry ?. "_" fish-mode-syntax-table)

  (define-key fish-mode-map (kbd "C-c C-c") #'executable-interpret)
  (define-key fish-mode-map (kbd "C-M-x")   #'sh-execute-region)

  (evil-leader/set-key-for-mode 'fish-mode
    "<tab> i" #'fish_indent))

(provide 'fish-cfg)
;;; dired-cfg.el ends here
