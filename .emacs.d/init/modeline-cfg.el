;;; modeline-cfg.el --- modeline setup

;;; Commentary:
;;; Code:

(setq-default
 sml/no-confirm-load-theme         t
 powerline-default-separator       'slant
 powerline-buffer-size-suffix      nil
 mode-line-in-non-selected-windows nil
 display-time-24hr-format          t
 display-time-day-and-date         t
 display-time-default-load-average nil
 display-time-format               "[%T]"
 display-time-interval             30)

(sml/setup)
(powerline-center-evil-theme)

(provide 'modeline-cfg)
;;; modeline-cfg.el ends here
