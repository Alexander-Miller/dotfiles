;;; modeline-cfg.el --- modeline setup

;;; Commentary:
;;; Code:

(require 'spaceline-config)
(spaceline-toggle-anzu-on)
(spaceline-toggle-evil-state-on)
(spaceline-toggle-buffer-modified-on)
(spaceline-toggle-selection-info-on)
(spaceline-helm-mode)

(setq
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

(spaceline-define-segment m-modes
  "Minor modes segment."
  (-non-nil
   (list
    (when (bound-and-true-p elpy-mode)          "🐍")
    (when (bound-and-true-p projectile-mode)    (format "P[%s]" (projectile-project-name)))
    (when (bound-and-true-p rainbow-mode)       "🌈")
    (when (bound-and-true-p auto-revert-mode)   "")
    (when (bound-and-true-p flycheck-mode)      "Ⓕ")
    (when (bound-and-true-p smartparens-mode)   "()")
    (when (bound-and-true-p flyspell-mode)      "Sp")
    (when (bound-and-true-p company-candidates) company-lighter))))

(spaceline-define-segment selection-placeholder
  "Empty space for when selection info is off."
  "       "
  :when (not (evil-visual-state-p)))

(defface spaceline-evil-volume
  `((t (:foreground "#000000" :background "#446677" :inherit mode-line)))
  "Spaceline's face for evil volume state.")
(defface spaceline-evil-treemacs
  ;; `((t (:foreground "#00ff00" :background "#ff0000" :inherit mode-line)))
  `((t (:foreground "#000000" :background "#446677" :inherit mode-line)))
  "Spaceline's face for treemacs state.")

(add-to-list 'spaceline-evil-state-faces '(volume   . spaceline-evil-volume))
(add-to-list 'spaceline-evil-state-faces '(treemacs . avy-lead-face-0))

(spaceline-install "a"
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
     m-modes
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

(setq-default mode-line-format
              '("%e" (:eval (spaceline-ml-a))))

(provide 'modeline-cfg)
;;; modeline-cfg.el ends here
