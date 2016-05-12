;;; cpp-cfg.el --- cpp config

;;; Commentary:
;;; Code:

(defun cpp-hook ()
  (irony-mode)
  (flycheck-irony-setup)
  (flycheck-select-checker 'irony)
  (rainbow-mode -1)
  (push '(case-label . 4) c-offsets-alist)
  (setq-local company-backends '((company-irony company-yasnippet company-files)))
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save)))

(add-hook 'c++-mode-hook #'cpp-hook t)
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

(with-eval-after-load "cc-mode"

  (setq-default
   c-basic-offset  4
   c-default-style "linux")

  (evil-leader/set-key-for-mode 'c++-mode
    "<tab> 1" #'rtags-start-process-maybe
    "<tab> 0" #'rtags-quit-rdm
    "<tab> r" #'rtags-rename-symbol
    "<tab> s" #'rtags-find-symbol
    "<tab> f" #'rtags-find-references-at-point
    "<tab> F" #'rtags-find-references
    "<tab> v" #'rtags-find-virtuals-at-point
    "<tab> i" #'rtags-print-symbol-info
    "<tab> t" #'rtags-symbol-type
    "<tab> j" #'rtags-next-match
    "<tab> k" #'rtags-previous-match
    "<tab> p" #'rtags-preprocess-file
    "<tab> d" #'rtags-print-dependencies)

  (evil-define-key 'normal c++-mode-map
    (kbd "M-.")   #'rtags-find-symbol-at-point
    (kbd "M-,")   #'rtags-location-stack-back
    (kbd "M-S-,") #'rtags-location-stack-forward
    (kbd "<tab>") #'c-indent-line-or-region)

  (evil-set-initial-state 'rtags-mode 'motion)

  (evil-define-key 'motion rtags-mode-map
    (kbd "<tab>") #'rtags-select-other-window
    (kbd "RET")   #'rtags-select-and-remove-rtags-buffer))

(provide 'cpp-cfg)
;;; cpp-cfg.el ends here
