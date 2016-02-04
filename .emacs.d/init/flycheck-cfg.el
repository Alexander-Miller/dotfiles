;;; flycheck-cfg.el --- flycheck config

;;; Commentary:
;;; Code:

(defun flycheck-activate () (flycheck-mode t))
(defconst flycheck-activation-hooks '(sh-mode-hook python-mode-hook c++-mode-hook rust-mode-hook))
(dolist (mode-hook flycheck-activation-hooks) (add-hook mode-hook #'flycheck-activate))

(with-eval-after-load "flycheck"

  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  (add-hook 'flycheck-mode-hook (lambda () (flycheck-pos-tip-mode)))

  (setq-default
   flycheck-check-syntax-automatically '(mode-enabled save idle-change)
   flycheck-checker-error-threshold    200
   flycheck-display-errors-delay       1
   flycheck-idle-change-delay          5
   flycheck-indication-mode            'left-fringe)

  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "q")     #'quit-window)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "f")     #'flycheck-error-list-set-filter)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "F")     #'flycheck-error-list-reset-filter)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "g")     #'flycheck-error-list-refresh)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "s")     #'tabulated-list-sort)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "<tab>") #'flycheck-error-list-goto-error)

  (evil-leader/set-key
    "C-y" (lambda () (interactive) (call-interactively 'flycheck-mode))
    "y y" #'flycheck-buffer
    "y Y" #'flycheck-copy-errors-as-kill
    "y s" #'flycheck-select-checker
    "y d" #'flycheck-describe-checker
    "y x" #'flycheck-disable-checker
    "y e" #'flycheck-set-checker-executable
    "y l" #'flycheck-list-errors
    "y j" #'flycheck-next-error
    "y 0" #'flycheck-first-error
    "y k" #'flycheck-previous-error
    "y v" #'flycheck-verify-setup))

(provide 'flycheck-cfg)
;;; flycheck-cfg.el ends here
