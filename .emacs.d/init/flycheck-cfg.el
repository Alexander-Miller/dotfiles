;;; flycheck-cfg.el --- flycheck config

;;; Commentary:
;;; Code:

(with-eval-after-load "flycheck"

  (setq-default
   flycheck-idle-change-delay          4
   flycheck-indication-mode            'right-fringe
   flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq-default flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(provide 'flycheck-cfg)
;;; flycheck-cfg.el ends here
