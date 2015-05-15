;;; dired-cfg.el --- dired config

;;; Commentary:
;;; Code:

(with-eval-after-load "dired"
  (diredp-toggle-find-file-reuse-dir 1)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "h" 'diredp-up-directory-reuse-dir-buffer)
  (evil-define-key 'normal dired-mode-map (kbd "RET")
     '(lambda () (interactive) (call-process-shell-command (concat "xdg-open " (shell-quote-argument (dired-get-filename))))))
  ;; TODO
  ;; (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  ;; (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  ;; (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
  ;; (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
  ;; (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
  ;; (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
  ;; (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  ;; (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  ;; (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  ;; (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  ;; (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default
   diredp-wrap-around-flag t
   diredp-hide-details-initially-flag t))

(provide 'dired-cfg)
;;; dired-cfg.el ends here
