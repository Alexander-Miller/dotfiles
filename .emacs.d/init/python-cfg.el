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

(with-eval-after-load "python"

  (elpy-enable)

  (defun elpy-doc-popup ()
  "Show documentation popup for the symbol at point."
  (interactive)
  (let ((symbol-at-point nil)
        (doc nil))
    (when (not current-prefix-arg)
      (setq doc (elpy-rpc-get-docstring))
      (when (not doc)
        (save-excursion
          (python-nav-backward-up-list)
          (setq doc (elpy-rpc-get-docstring))))
      (when (not doc)
        (setq doc (elpy-rpc-get-pydoc-documentation
                   (elpy-doc--symbol-at-point))))
      (when (not doc)
        (save-excursion
          (python-nav-backward-up-list)
          (setq doc (elpy-rpc-get-pydoc-documentation
                     (elpy-doc--symbol-at-point))))))
    (when (not doc)
      (setq doc (elpy-rpc-get-pydoc-documentation
                 (elpy-doc--read-identifier-from-minibuffer
                  (elpy-doc--symbol-at-point)))))
    (if doc (pos-tip-show doc nil nil nil -1)
      (error "No documentation found"))))

  (define-key python-mode-map (kbd "C-ä") 'elpy-doc-popup)

  (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (remove 'elpy-module-highlight-indentation elpy-modules))

  (setq-default elpy-rpc-backend "jedi"))

(provide 'python-cfg)
;;; python-cfg.el ends here
