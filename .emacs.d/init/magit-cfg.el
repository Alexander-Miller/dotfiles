;;; magit-cfg.el --- magit config

;;; Commentary:
;;; Code:

(evil-leader/set-key "gs" #'magit-status)

(with-eval-after-load 'magit

  (evil-magit-init)

  (setq
   git-commit-summary-max-length       999
   magit-diff-show-lines-boundary      nil)

  (defvar a/magit-key-maps
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

  (a/def-key-for-maps (kbd "K")   #'a/quick-backward                    a/magit-key-maps)
  (a/def-key-for-maps [escape]    #'keyboard-quit                       a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-j") #'magit-section-forward-sibling       a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-k") #'magit-section-backward-sibling      a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-0") #'eyebrowse-switch-to-window-config-0 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-1") #'eyebrowse-switch-to-window-config-1 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-2") #'eyebrowse-switch-to-window-config-2 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-3") #'eyebrowse-switch-to-window-config-3 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-4") #'eyebrowse-switch-to-window-config-4 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-5") #'eyebrowse-switch-to-window-config-5 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-6") #'eyebrowse-switch-to-window-config-6 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-7") #'eyebrowse-switch-to-window-config-7 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-8") #'eyebrowse-switch-to-window-config-8 a/magit-key-maps)
  (a/def-key-for-maps (kbd "M-9") #'eyebrowse-switch-to-window-config-9 a/magit-key-maps))

(provide 'magit-cfg)
;;; magit-cfg.el ends here
