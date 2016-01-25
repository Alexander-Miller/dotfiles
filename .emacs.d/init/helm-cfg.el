;;; helm-cfg.el --- helm config

;;; Commentary:
;;; Code:

(helm-mode t)
(helm-flx-mode t)

(setq-default
 helm-adaptive-mode                     t
 helm-ag-base-command                   "ag -f --hidden --nocolor --nogroup --depth 999999"
 helm-apropos-fuzzy-match               t
 helm-autoresize-mode                   t
 helm-autoresize-max-height             50
 helm-autoresize-min-height             50
 helm-buffers-fuzzy-matching            t
 helm-candidate-number-limit            100
 helm-completion-in-region-fuzzy-match  t
 helm-completion-window-scroll-margin   5
 helm-display-source-at-screen-top      nil
 helm-echo-input-in-header-line         t
 helm-ff-auto-update-initial-value      nil
 helm-ff-file-name-history-use-recentf  t
 helm-ff-search-library-in-sexp         t
 helm-ff-transformer-show-only-basename t
 helm-file-cache-fuzzy-match            t
 helm-idle-delay                        0.1
 helm-imenu-fuzzy-match                 t
 helm-input-idle-delay                  0.1
 helm-kill-ring-max-lines-number        5
 helm-locate-fuzzy-match                nil
 helm-M-x-fuzzy-match                   t
 helm-move-to-line-cycle-in-source      t
 helm-projectile-fuzzy-match            t
 helm-quick-update                      t
 helm-recentf-fuzzy-match               t
 helm-scroll-amount                     5
 helm-semantic-fuzzy-match              t
 helm-split-window-default-side         'right
 helm-for-files-preferred-list          '(helm-source-buffers-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-file-cache
                                          helm-source-files-in-current-dir
                                          helm-source-locate
                                          helm-source-buffer-not-found))

(add-to-list
   'shackle-rules
   '("*helm for files*"  :select t :align 'below :size 0.45)
   '("*Helm Find Files*" :select t :align 'below :size 0.30))

(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

(provide 'helm-cfg)
;; helm-cfg.el ends here
