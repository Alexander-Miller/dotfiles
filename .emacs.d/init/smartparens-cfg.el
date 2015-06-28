;;; smartparens-cfg.el --- paren management config

;;; Commentary:
;;; Code:

(require 'smartparens-config)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(smartparens-global-strict-mode t)
(show-smartparens-global-mode t)

(setq-default
 sp-show-pair-from-inside t
 sp-show-pair-delay       0.1)

(evil-leader/set-key
  "s f" 'sp-forward-slurp-sexp
  "s F" 'sp-forward-barf-sexp
  "s b" 'sp-backward-slurp-sexp
  "s B" 'sp-backward-barf-sexp)

(provide 'smartparens-cfg)
;;; smartparens-cfg.el ends here
