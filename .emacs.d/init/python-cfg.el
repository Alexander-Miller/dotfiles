;;; python-cfgel --- python config

;;; Commentary:
;;; Code:

(add-hook 'python-mode-hook
          '(lambda () (setq-local company-backends
                             '((elpy-company-backend company-yasnippet company-files company-dabbrev-code)))))

(with-eval-after-load 'python-mode
  (setq-default elpy-rpc-backend "jedi")
  (elpy-enable))

(provide 'python-cfg)
;;; python-cfg.el ends here
