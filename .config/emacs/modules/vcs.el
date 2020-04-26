;; -*- lexical-binding: t -*-

(std::using-packages
 magit
 evil-magit
 forge)

(std::autoload vcs
  #'std::vcs::with-editor-hook
  #'std::vcs::org-reveal-on-visit
  #'std::vcs::magit-pkg-status)

(std::keybind
 :leader
 "gs"  #'magit-status
 "gp"  #'std::vcs::magit-pkg-status
 "gfl" #'magit-log-buffer-file)
;; (spacemacs/set-leader-keys TODO
;;  "gb"  'spacemacs/git-blame-micro-state
;;  "gc"  'magit-clone
;;  "gfF" 'magit-find-file
;;  "gfl" 'magit-log-buffer-file
;;  "gfd" 'magit-diff
;;  "gi"  'magit-init
;;  "gL"  'magit-list-repositories
;;  "gm"  'magit-dispatch
;;  "gs"  'magit-status
;;  "gS"  'magit-stage-file
;;  "gU"  'magit-unstage-file)
;; Settings
(std::after magit

  (evil-magit-init)

  (std::add-hook 'with-editor-mode-hook
    (rainbow-delimiters-mode -1)
    (rainbow-mode -1))

  (add-hook 'magit-diff-visit-file-hook #'std::vcs::org-reveal-on-visit)

  (setf
   magit-display-buffer-function              #'magit-display-buffer-fullframe-status-v1
   magit-repository-directories               '(("~/Documents/git/" . 1))
   magit-save-repository-buffers              'dontask
   git-commit-summary-max-length              120
   magit-section-visibility-indicator         nil
   magit-diff-highlight-hunk-region-functions '(magit-diff-highlight-hunk-region-using-face)))
;TODO(2020/04/16): check rainbow
;; Keybinds
(std::after magit

  (std::keybind
    :keymap
    (magit-mode-map
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
     magit-unstaged-section-map)
    "SPC" nil
    "J"   #'std::evil::forward-five-lines
    "K"   #'std::evil::backward-five-lines
    "M-j" #'magit-section-forward-sibling
    "M-k" #'magit-section-backward-sibling
    ",u"  #'magit-section-up
    ",1"  #'magit-section-show-level-1-all
    ",2"  #'magit-section-show-level-2-all
    ",3"  #'magit-section-show-level-3-all
    ",4"  #'magit-section-show-level-4-all
    "M-1" #'winum-select-window-1
    "M-2" #'winum-select-window-2
    "M-3" #'winum-select-window-3
    "M-4" #'winum-select-window-4))
