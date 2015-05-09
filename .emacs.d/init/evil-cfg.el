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

(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1   "#ab3737")
             (set-face-background 'powerline-inactive1 "#ab3737")
             (set-face-background 'sml/vc-edited       "#ab3737")
             (set-face-background 'sml/vc              "#ab3737")
             (aggressive-indent-mode 0)
             (powerline-reset)))

(add-hook 'evil-emacs-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1   "#444444")
             (set-face-background 'powerline-inactive1 "#444444")
             (set-face-background 'sml/vc-edited       "#444444")
             (set-face-background 'sml/vc              "#444444")
             (my/aggressive-indent-if)
             (powerline-reset)))

(add-hook 'evil-visual-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1   "#634566")
             (set-face-background 'powerline-inactive1 "#634566")
             (set-face-background 'sml/vc-edited       "#634566")
             (set-face-background 'sml/vc              "#634566")
             (aggressive-indent-mode 0)
             (powerline-reset)))

(add-hook 'evil-insert-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1   "#3d5837")
             (set-face-background 'powerline-inactive1 "#3d5837")
             (set-face-background 'sml/vc              "#3d5837")
             (set-face-background 'sml/vc-edited       "#3d5837")
             (my/aggressive-indent-if)
             (powerline-reset)))

(provide 'evil-cfg)
;;; evil-cfg.el ends here
