;;; evil-cfg.el --- evil config

;;; Commentary:
;;; Code:

(if (not (bound-and-true-p global-evil-leader-mode))
    (evil-leader/set-leader "<SPC>"))

(global-evil-leader-mode 1)
(evil-mode 1)
(global-evil-matchit-mode 1)

(add-hook 'evil-emacs-state-entry-hook
          (lambda () (setq-local cursor-type '(bar . 1))))

(add-hook 'evil-operator-state-entry-hook
          (lambda () (setq-local cursor-type '(hbar . 5))))

(setq-default
 evil-normal-state-cursor   '("#ab3737" box)
 evil-insert-state-cursor   '("#33aa33" bar)
 evil-motion-state-cursor   '("#c97449" box)
 evil-operator-state-cursor '("#cc6666" '(hbar . 5))
 evil-emacs-state-cursor    '("#339999" bar)
 evil-resize-state-cursor   '("#ffdb1a" box)
 evil-default-state         'normal
 evil-auto-indent           0
 evil-shift-width           4
 evil-repeat-move-cursor    0
 evil-find-skip-newlines    t)

(provide 'evil-cfg)
;;; evil-cfg.el ends here
