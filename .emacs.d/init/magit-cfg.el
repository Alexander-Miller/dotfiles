;;; magit-cfg.el --- magit config

;;; Commentary:
;;; Code:

(setq-default magit-last-seen-setup-instructions "1.4.0")

(with-eval-after-load 'magit

  (setq-default
   magit-highlight-trailing-whitespace t
   magit-key-mode-show-usage           t
   magit-revert-backup                 t
   magit-revert-item-confirm           t
   magit-stage-all-confirm             t
   magit-use-overlays                  t)

  (define-key magit-status-mode-map (kbd "C-v") 'set-mark-command)

  (my/def-key-for-maps
   (kbd "j") 'next-line (list magit-status-mode-map magit-log-mode-map magit-diff-mode-map magit-commit-mode-map))
  (my/def-key-for-maps
   (kbd "k") 'previous-line (list magit-status-mode-map magit-log-mode-map magit-diff-mode-map magit-commit-mode-map))

  (my/def-key-for-maps
   (kbd "J") 'my/quick-forward (list magit-status-mode-map magit-log-mode-map magit-diff-mode-map magit-commit-mode-map))
  (my/def-key-for-maps
   (kbd "K") 'my/quick-backward (list magit-status-mode-map magit-log-mode-map magit-diff-mode-map magit-commit-mode-map))

  (my/def-key-for-maps
   (kbd "C-k") 'magit-discard-item (list magit-status-mode-map))

  (my/def-key-for-maps
   (kbd "M-j") 'magit-goto-next-sibling-section (list magit-status-mode-map magit-diff-mode-map))
  (my/def-key-for-maps
   (kbd "M-k") 'magit-goto-previous-sibling-section (list magit-status-mode-map magit-diff-mode-map)))

(provide 'magit-cfg)
;;; magit-cfg.el ends here
