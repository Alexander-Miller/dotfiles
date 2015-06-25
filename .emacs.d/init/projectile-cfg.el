;;; projectile-cfg --- projectile config

;;; Commentary:
;;; Code:

(with-eval-after-load "projectile"

  (helm-projectile-on)
  (setq-default projectile-completion-system 'helm)

  (evil-leader/set-key
    "p f" 'projectile-find-file
    "p a" 'helm-projectile-ag))

(provide 'projectile-cfg)
;;; projectile-cfg.el ends here
