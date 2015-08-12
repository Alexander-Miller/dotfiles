;;; smartparens-cfg.el --- paren management config

;;; Commentary:
;;; Code:

(require 'smartparens-config)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(smartparens-global-strict-mode t)
(show-smartparens-global-mode t)

(setq-default
 sp-autodelete-pair         t
 sp-autodelete-wrap         t
 sp-autodelete-closing-pair t
 sp-autodelete-opening-pair t
 sp-autoinsert-pair         t
 sp-autowrap-region         t
 sp-show-pair-from-inside   t
 sp-show-pair-delay         0.1
 sp-use-subword             t)

(define-key smartparens-strict-mode-map [remap sp-delete-char] 'delete-char)

(sp-pair "<" ">")
(define-key evil-visual-state-map (kbd "(")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
(define-key evil-visual-state-map (kbd "[")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
(define-key evil-visual-state-map (kbd "{")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
(define-key evil-visual-state-map (kbd "<")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "<")))
(define-key evil-visual-state-map (kbd "\"") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))

(evil-leader/set-key
  "s f" 'sp-forward-slurp-sexp
  "s F" 'sp-forward-barf-sexp
  "s b" 'sp-backward-slurp-sexp
  "s B" 'sp-backward-barf-sexp
  "s u" 'sp-unwrap-sexp
  "s r" 'sp-rewrap-sexp)

(provide 'smartparens-cfg)
;;; smartparens-cfg.el ends here
