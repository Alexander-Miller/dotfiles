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

(defvar hl-line-was-on nil)

(defun visual-entry-hook ()
  "Evil visual state entry hook."
  (if global-hl-line-mode
      (progn
        (setq-local hl-line-was-on t)
        (setq-local global-hl-line-mode nil))))

(defun visual-exit-hook ()
  "Evil visual state exit hook."
  (if hl-line-was-on
      (progn
        (setq-local hl-line-was-on nil)
        (setq-local global-hl-line-mode t))))

(add-hook 'evil-visual-state-entry-hook #'visual-entry-hook)
(add-hook 'evil-visual-state-exit-hook  #'visual-exit-hook)

(provide 'evil-cfg)
;;; evil-cfg.el ends here
