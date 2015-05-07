;;; shell-cfg.el --- emacs shell configuration

;;; Commentary:

;;; Code:

(add-hook 'term-mode-hook
          '(lambda ()
             (setq-local company-backends '())
             (nlinum-mode 0)
             (setq-local scroll-margin 0)))

(global-set-key [f1] 'multi-term-dedicated-toggle)

(with-eval-after-load "term"
  (setq-default
   term-buffer-maximum-size                 10000
   term-suppress-hard-newline               nil
   multi-term-program                       "/usr/bin/fish"
   multi-term-dedicated-buffer-name         "Dedicated Terminal"
   multi-term-scroll-to-bottom-on-output    t
   multi-term-switch-after-close            nil
   multi-term-dedicated-window-height       33
   multi-term-dedicated-max-window-height   33
   multi-term-dedicated-select-after-open-p t))

(provide 'shell-cfg)
;;; shell-cfg.el ends here
