;;; flycheck-cfg.el --- flycheck config

;;; Commentary:
;;; Code:

(with-eval-after-load "flycheck"


  (defun flycheck-pos-tip-error-messages (errors)
    "Display flycheck's ERRORS with pos-tip."
    (-when-let (messages (-keep #'flycheck-error-message errors))
      (pos-tip-show (mapconcat 'identity messages "\n\n") nil nil nil -1)))

  (setq-default
   flycheck-idle-change-delay          4
   flycheck-display-errors-function    #'flycheck-pos-tip-error-messages
   flycheck-indication-mode            'right-fringe
   flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq-default flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(provide 'flycheck-cfg)
;;; flycheck-cfg.el ends here
