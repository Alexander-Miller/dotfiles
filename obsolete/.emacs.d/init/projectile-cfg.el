;;; projectile-cfg --- projectile config

;;; Commentary:
;;; Code:

(projectile-global-mode t)

(helm-projectile-on)

(setq
 helm-projectile-fuzzy-match  t
 projectile-completion-system 'helm
 projectile-mode-line         '(:eval (format " P[%s]" (projectile-project-name))))

(evil-leader/set-key
  "p f"   'helm-projectile-find-file
  "p F"   'helm-projectile-find-file-in-known-projects
  "p a"   'helm-projectile-ag
  "p d"   'projectile-dired
  "p C-D" 'helm-projectile-find-dir
  "p l"   'helm-projectile-switch-to-buffer
  "p p"   'helm-projectile-switch-project
  "p g"   'projectile-vc
  "p C-C" 'projectile-compile-project
  "p C-T" 'projectile-test-project
  "p C-R" 'projectile-run-shell-command-in-root
  "p r"   'projectile-replace
  "p t f" 'projectile-find-tag
  "p t r" 'projectile-regenerate-tags
  "p c f" 'projectile-cache-current-file
  "p c i" 'projectile-invalidate-cache)

(provide 'projectile-cfg)
;;; projectile-cfg.el ends here
