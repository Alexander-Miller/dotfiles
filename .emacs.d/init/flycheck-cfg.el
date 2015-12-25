;;; flycheck-cfg.el --- flycheck config

;;; Commentary:
;;; Code:

(defun flycheck-activate () (flycheck-mode t))
(defconst flycheck-activation-hooks '(sh-mode-hook python-mode-hook c++-mode-hook rust-mode-hook))
(dolist (mode-hook flycheck-activation-hooks) (add-hook mode-hook #'flycheck-activate))

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
