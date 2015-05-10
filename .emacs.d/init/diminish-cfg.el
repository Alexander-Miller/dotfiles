;;; diminish-cfg.el --- diminish config

;;; Commentary:
;;; Code:

(diminish 'helm-mode         "")
(diminish 'golden-ratio-mode "")
(diminish 'git-gutter-mode   " ")
(diminish 'undo-tree-mode    " ")
(diminish 'visual-line-mode  "")
(diminish 'subword-mode      "")
(diminish 'anaconda-mode     " 🐍")
(diminish 'company-mode      " ")
(with-eval-after-load "rainbow-mode"
  (diminish 'rainbow-mode "🌈"))
(with-eval-after-load "magit"
  (diminish 'magit-auto-revert-mode ""))
(with-eval-after-load "aggressive-indent"
  (diminish 'aggressive-indent-mode " ➠"))

(provide'diminish-cfg)
;;; diminish-cfg.el ends here
