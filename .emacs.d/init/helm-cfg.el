;;; helm-cfg.el --- helm config

;;; Commentary:
;;; Code:

(helm-mode 1)

(setq-default
 helm-ag-base-command                   "ag -f --hidden --nocolor --nogroup --depth -1"
 helm-echo-input-in-header-line         t
 helm-M-x-fuzzy-match                   t
 helm-semantic-fuzzy-match              t
 helm-imenu-fuzzy-match                 t
 helm-apropos-fuzzy-match               t
 helm-buffers-fuzzy-matching            t
 helm-completion-in-region-fuzzy-match  t
 helm-file-cache-fuzzy-match            t
 helm-projectile-fuzzy-match            t
 helm-semantic-fuzzy-match              t
 helm-locate-fuzzy-match                t
 helm-recentf-fuzzy-match               t
 helm-candidate-number-limit            100
 helm-move-to-line-cycle-in-source      t
 helm-scroll-amount                     5
 helm-ff-file-name-history-use-recentf  t
 helm-autoresize-mode                   t
 helm-autoresize-max-height             50
 helm-autoresize-min-height             50
 helm-adaptive-mode                     t
 helm-ff-search-library-in-sexp         t
 helm-quick-update                      t
 helm-idle-delay                        0.1
 helm-input-idle-delay                  0.1
 helm-display-source-at-screen-top      nil
 helm-completion-window-scroll-margin   5
 helm-split-window-default-side         'right
 helm-kill-ring-max-lines-number        5
 helm-default-external-file-browser     "urxvt -e ranger"
 helm-ff-transformer-show-only-basename t
 helm-ff-auto-update-initial-value      nil)

;; (defadvice helm-display-mode-line (after undisplay-header activate)
  ;; "Will remove unnecessary helm header."
  ;; (setq header-line-format nil))

(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

(provide 'helm-cfg)
;;; helm-cfg.el ends here
