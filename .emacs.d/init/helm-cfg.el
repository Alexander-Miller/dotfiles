;;; helm-cfg.el --- helm config

;;; Commentary:
;;; Code:

(helm-mode 1)

(setq-default
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

(defadvice helm-display-mode-line (after undisplay-header activate)
  "Will remove unnecessary helm header."
  (setq header-line-format nil))

;; helm and global keys
(define-key helm-map            (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map            (kbd "C-,")   'helm-select-action)
(define-key helm-map            (kbd "C-j")   'helm-next-line)
(define-key helm-map            (kbd "C-k")   'helm-previous-line)
(define-key helm-map            (kbd "M-j")   'helm-next-source)
(define-key helm-map            (kbd "M-k")   'helm-previous-source)
(define-key helm-find-files-map (kbd "C-d")   'helm-ff-persistent-delete)
(define-key helm-buffer-map     (kbd "C-d")   'helm-buffer-run-kill-persistent)
(global-set-key                 (kbd "M-x")   'helm-M-x)
(global-set-key                 (kbd "C-x b") 'my/helm-mini-below)

(provide 'helm-cfg)
;;; helm-cfg.el ends here
