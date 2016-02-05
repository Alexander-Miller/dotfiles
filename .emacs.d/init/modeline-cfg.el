;;; modeline-cfg.el --- modeline setup

;;; Commentary:
;;; Code:

(require 'spaceline-config)
(spaceline-toggle-anzu-on)
(spaceline-toggle-evil-state-on)
(spaceline-toggle-buffer-modified-on)
(spaceline-toggle-selection-info-on)

(setq-default
 anzu-cons-mode-line-p               nil
 spaceline-highlight-face-func       'spaceline-highlight-face-evil-state
 spaceline-minor-modes-separator     " "
 spaceline-workspace-numbers-unicode t
 powerline-height                    24
 powerline-default-separator         'wave
 powerline-buffer-size-suffix        t)

(spaceline-define-segment anzu-placeholder
  "Empty space for when anzu is off."
  "     "
  :when (not (and active (bound-and-true-p anzu--state))))

(spaceline-define-segment selection-placeholder
  "Empty space for when selection info is off."
  "       "
  :when (not (evil-visual-state-p)))

(spaceline-install

   '(((workspace-number window-number)
      :fallback evil-state
      :separator "|"
      :face highlight-face)
     anzu
     anzu-placeholder
     (buffer-modified buffer-size buffer-id remote-host)
     major-mode
     ((flycheck-error flycheck-warning flycheck-info)
      :when active)
     (((minor-modes :separator spaceline-minor-modes-separator)
       process)
      :when active)
     (erc-track :when active)
     (version-control :when active)
     (org-pomodoro :when active)
     (org-clock :when active)
     nyan-cat)

   `((battery :when active)
     selection-info
     selection-placeholder
     ((buffer-encoding-abbrev
       point-position
       line-column)
      :separator " | ")
     (global :when active)
     ;; ,@additional-segments
     buffer-position
     hud))

(provide 'modeline-cfg)
;;; modeline-cfg.el ends here
