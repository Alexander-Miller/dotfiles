;;; magit-cfg.el --- magit config

;;; Commentary:
;;; Code:

(with-eval-after-load 'magit

  (setq-default
   git-commit-summary-max-length       100
   magit-highlight-trailing-whitespace t
   magit-diff-show-lines-boundary      t
   magit-key-mode-show-usage           t
   magit-revert-backup                 t
   magit-revert-item-confirm           t
   magit-stage-all-confirm             t
   magit-use-overlays                  t)

  (evil-set-initial-state 'magit-popup-mode 'emacs)

  (define-key magit-status-mode-map (kbd "C-v") 'set-mark-command)

  (defvar my/magit-key-maps
    (list
     magit-status-mode-map
     magit-log-mode-map
     magit-diff-mode-map
     magit-branch-section-map
     magit-untracked-section-map
     magit-file-section-map
     magit-status-mode-map
     magit-hunk-section-map
     magit-stash-section-map
     magit-stashes-section-map
     magit-staged-section-map
     magit-unstaged-section-map))

  (my/def-key-for-maps (kbd "j") 'next-line     my/magit-key-maps)
  (my/def-key-for-maps (kbd "k") 'previous-line my/magit-key-maps)

  (my/def-key-for-maps (kbd "J") 'my/quick-forward  my/magit-key-maps)
  (my/def-key-for-maps (kbd "K") 'my/quick-backward my/magit-key-maps)

  (my/def-key-for-maps (kbd "C-k") 'magit-discard my/magit-key-maps)

  (my/def-key-for-maps (kbd "M-j") 'magit-goto-next-sibling-section my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-k") 'magit-goto-previous-sibling-section my/magit-key-maps)

  (evil-add-hjkl-bindings git-rebase-mode-map 'normal
    (kbd "M-j") #'git-rebase-move-line-down
    (kbd "M-k") #'git-rebase-move-line-up
    "J" #'my/quick-forward
    "K" #'my/quick-backward
    "D" #'git-rebase-kill-line
    "o" #'git-rebase-show-commit
    "s" #'git-rebase-squash
    "f" #'git-rebase-fixup
    "p" #'git-rebase-pick
    "e" #'git-rebase-edit
    "r" #'git-rebase-reword)

  )

(provide 'magit-cfg)
;;; magit-cfg.el ends here
