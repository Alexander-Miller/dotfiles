;;; diminish-cfg.el --- diminish config

;;; Commentary:
;;; Code:

(diminish 'helm-mode "")
(diminish 'golden-ratio-mode "GR")
(diminish 'git-gutter-mode "GG")
(diminish 'undo-tree-mode "UT")
(diminish 'visual-line-mode "VL")
(with-eval-after-load "aggressive-indent"
  (diminish 'aggressive-indent-mode " ➠"))

(provide'diminish-cfg)
;;; diminish-cfg.el ends here
