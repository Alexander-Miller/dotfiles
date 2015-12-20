;;; flycheck-cfg.el --- flycheck config

;;; Commentary:
;;; Code:

(add-hook 'sh-mode-hook (flycheck-mode t))

(add-hook 'flycheck-mode-hook (lambda () (flycheck-pos-tip-mode)))

(with-eval-after-load "flycheck"

  (setq-default
   flycheck-keymap-prefix (kbd "C-c f")
   flycheck-idle-change-delay          4
   flycheck-indication-mode            'left-fringe
   flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(provide 'flycheck-cfg)
;;; flycheck-cfg.el ends here
