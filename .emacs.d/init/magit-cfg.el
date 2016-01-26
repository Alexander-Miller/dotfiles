;;; magit-cfg.el --- magit config

;;; Commentary:
;;; Code:

(with-eval-after-load 'magit

  (evil-magit-init)

  (setq-default
   git-commit-summary-max-length       100
   magit-highlight-trailing-whitespace t
   magit-diff-show-lines-boundary      nil
   magit-key-mode-show-usage           t
   magit-revert-backup                 t
   magit-revert-item-confirm           t
   magit-stage-all-confirm             t
   magit-use-overlays                  t)

  (defvar my/magit-key-maps
    (list
     magit-mode-map
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

  (my/def-key-for-maps (kbd "K")   #'my/quick-backward                   my/magit-key-maps)
  (my/def-key-for-maps [escape]    #'keyboard-quit                       my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-j") #'magit-section-forward-sibling       my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-k") #'magit-section-backward-sibling      my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-0") #'eyebrowse-switch-to-window-config-0 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-1") #'eyebrowse-switch-to-window-config-1 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-2") #'eyebrowse-switch-to-window-config-2 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-3") #'eyebrowse-switch-to-window-config-3 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-4") #'eyebrowse-switch-to-window-config-4 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-5") #'eyebrowse-switch-to-window-config-5 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-6") #'eyebrowse-switch-to-window-config-6 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-7") #'eyebrowse-switch-to-window-config-7 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-8") #'eyebrowse-switch-to-window-config-8 my/magit-key-maps)
  (my/def-key-for-maps (kbd "M-9") #'eyebrowse-switch-to-window-config-9 my/magit-key-maps))

(provide 'magit-cfg)
;;; magit-cfg.el ends here
