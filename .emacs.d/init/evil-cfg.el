;;; evil-cfg.el --- evil config

;;; Commentary:
;;; Code:

(if (not (bound-and-true-p global-evil-leader-mode))
    (evil-leader/set-leader "<SPC>"))

(global-evil-leader-mode 1)
(evil-mode 1)
(global-evil-surround-mode 1)
(global-evil-matchit-mode 1)

(setq-default
 evil-default-state      'normal
 evil-auto-indent        0
 evil-shift-width        4
 evil-repeat-move-cursor 0
 evil-find-skip-newlines t)

(provide 'evil-cfg)
;;; evil-cfg.el ends here
