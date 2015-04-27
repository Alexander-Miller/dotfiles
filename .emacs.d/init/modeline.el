;;; modeline.el --- modeline setup

;;; Commentary:
;;; Code:

(setq-default
 sml/no-confirm-load-theme         t
 sml/theme                         'automatic
 powerline-default-separator       'wave
 powerline-buffer-size-suffix      nil
 mode-line-in-non-selected-windows nil
 display-time-24hr-format          t
 display-time-day-and-date         t
 display-time-default-load-average nil
 display-time-format               "[%T]"
 display-time-interval             30
 )

(sml/setup)
(display-time-mode t)
(powerline-center-evil-theme)

(provide 'modeline)
;;; modeline.el ends here
