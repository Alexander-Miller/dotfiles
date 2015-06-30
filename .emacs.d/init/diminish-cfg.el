;;; diminish-cfg.el --- diminish config

;;; Commentary:
;;; Code:

(diminish 'helm-mode         "")
(diminish 'golden-ratio-mode "")
(diminish 'subword-mode      "")
(diminish 'anzu-mode         "")
(diminish 'git-gutter-mode   " ")
(diminish 'undo-tree-mode    " ")
(diminish 'visual-line-mode  " ")
(diminish 'company-mode      " ")
(diminish 'yas-minor-mode    " ")
(with-eval-after-load 'anaconda-mode
  (diminish 'anaconda-mode " 🐍"))
(with-eval-after-load 'rainbow-mode
  (diminish 'rainbow-mode " 🌈"))
(with-eval-after-load 'magit-auto-revert-mode
  (diminish 'magit-auto-revert-mode " "))
(with-eval-after-load "aggressive-indent"
  (diminish 'aggressive-indent-mode " ➠"))
(with-eval-after-load "flyspell"
  (diminish 'flyspell-mode " ✈"))
(with-eval-after-load "smartparens"
  (diminish 'smartparens-mode " ()"))
(with-eval-after-load "evil-smartparens"
  (diminish 'evil-smartparens-mode " (E)"))
(with-eval-after-load "highlight-symbol"
  (diminish 'highlight-symbol-mode " $"))

(provide'diminish-cfg)
;;; diminish-cfg.el ends here
