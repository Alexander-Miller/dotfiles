;;; diminish-cfg.el --- diminish config

;;; Commentary:
;;; Code:

(diminish 'helm-mode         "")
(diminish 'golden-ratio-mode "")
(diminish 'subword-mode      "")
(diminish 'anzu-mode         "")
(diminish 'undo-tree-mode    "")
(diminish 'visual-line-mode  "")
(diminish 'yas-minor-mode    "")
(diminish 'shackle-mode      "")
(diminish 'git-gutter-mode   " ")
(diminish 'company-mode      " ⓒ")
(with-eval-after-load "elpy"
  (diminish 'elpy-mode " 🐍"))
(with-eval-after-load 'rainbow-mode
  (diminish 'rainbow-mode " 🌈"))
(with-eval-after-load "magit"
  (diminish 'global-auto-revert-mode " "))
(with-eval-after-load "aggressive-indent"
  (diminish 'aggressive-indent-mode " ➠"))
(with-eval-after-load "flyspell"
  (diminish 'flyspell-mode " ✈"))
(with-eval-after-load "smartparens"
  (diminish 'smartparens-mode " ()"))
(with-eval-after-load "evil-smartparens"
  (diminish 'evil-smartparens-mode ""))
(with-eval-after-load "highlight-symbol"
  (diminish 'highlight-symbol-mode ""))

(provide'diminish-cfg)
;;; diminish-cfg.el ends here
