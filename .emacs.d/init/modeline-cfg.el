;;; modeline-cfg.el --- modeline setup

;;; Commentary:
;;; Code:

(require 'spaceline-config)
(spaceline-spacemacs-theme)
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

(provide 'modeline-cfg)
;;; modeline-cfg.el ends here
