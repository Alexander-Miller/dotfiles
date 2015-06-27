;;; python-cfgel --- python config

;;; Commentary:
;;; Code:

(defun my/python-mode-hook ()
  "Python mode hook."
  (setq-local company-backends '((elpy-company-backend company-yasnippet company-files company-dabbrev-code))))

(defun my/inferior-python-mode-hook ()
  "Inferior Python mode hook."
  (setq-local company-backends '((company-capf))))

(add-hook 'python-mode-hook          #'my/python-mode-hook)
(add-hook 'inferior-python-mode-hook #'my/inferior-python-mode-hook)

(with-eval-after-load 'python-mode
  (setq-default elpy-rpc-backend "jedi")
  (elpy-enable))

(provide 'python-cfg)
;;; python-cfg.el ends here
