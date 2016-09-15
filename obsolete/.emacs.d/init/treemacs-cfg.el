;;; treemacs-cfg.el --- treemacs config

;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/Documents/git/treemacs")
(require 'treemacs)
(treemacs-evil-config)

(evil-leader/set-key
  "n n" #'treemacs-toggle)
(evil-leader/set-key-for-mode 'treemacs-mode
  "n n" #'treemacs-toggle)
(evil-leader/set-key
  "N N" #'treemacs-projectile-init)

(provide 'treemacs-cfg)
;;; treemacs-cfg.el ends here
