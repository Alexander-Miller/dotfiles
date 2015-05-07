;;; diminish-cfg.el --- diminish config

;;; Commentary:
;;; Code:

(diminish 'helm-mode "")
(diminish 'golden-ratio-mode "GR")
(diminish 'git-gutter-mode "GG")
(diminish 'undo-tree-mode "UT")
(diminish 'visual-line-mode "VL")
(diminish 'subword-mode "")
(diminish 'anaconda-mode "ANC")
(with-eval-after-load "magit"
  (diminish 'magit-auto-revert-mode "MR"))
(with-eval-after-load "aggressive-indent"
  (diminish 'aggressive-indent-mode " ➠"))

(provide'diminish-cfg)
;;; diminish-cfg.el ends here
