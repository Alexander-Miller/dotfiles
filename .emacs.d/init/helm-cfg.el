;;; helm-cfg.el --- helm config

;;; Commentary:
;;; Code:

(helm-mode t)
(helm-flx-mode t)
(helm-adaptive-mode)

(setq
 helm-ag-base-command                   "ag -f --hidden --nocolor --nogroup --depth 999999"
 helm-apropos-fuzzy-match               t
 helm-autoresize-mode                   nil
 helm-autoresize-max-height             50
 helm-autoresize-min-height             50
 helm-buffers-fuzzy-matching            t
 helm-candidate-number-limit            100
 helm-completion-in-region-fuzzy-match  t
 helm-completion-window-scroll-margin   5
 helm-display-header-line               nil
 helm-display-source-at-screen-top      t
 helm-echo-input-in-header-line         t
 helm-ff-auto-update-initial-value      nil
 helm-ff-file-name-history-use-recentf  t
 helm-ff-search-library-in-sexp         t
 helm-ff-transformer-show-only-basename t
 helm-file-cache-fuzzy-match            t
 helm-flx-limit                         100
 helm-idle-delay                        0.05
 helm-imenu-fuzzy-match                 t
 helm-input-idle-delay                  0.05
 helm-kill-ring-max-lines-number        5
 helm-locate-fuzzy-match                nil
 helm-mode-fuzzy-match                  t
 helm-M-x-fuzzy-match                   t
 helm-move-to-line-cycle-in-source      t
 helm-quick-update                      t
 helm-recentf-fuzzy-match               t
 helm-scroll-amount                     5
 helm-semantic-fuzzy-match              t
 helm-split-window-default-side         'below
 helm-split-window-in-side-p            t
 helm-yas-display-key-on-candidate      t
 helm-yas-not-display-dups              nil
 helm-for-files-preferred-list          '(helm-source-buffers-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-file-cache
                                          helm-source-files-in-current-dir
                                          helm-source-locate
                                          helm-source-buffer-not-found))

(evil-leader/set-key
  "f f" 'helm-find-files
  "h y" '(lambda () (interactive) (helm-c-yas-complete) (evil-insert-state))
  "h h" 'helm-apropos
  "h i" 'helm-semantic-or-imenu
  "h s" 'helm-swoop
  "h S" 'helm-multi-swoop
  "h a" 'helm-ag
  "h A" 'helm-do-ag
  "h r" 'helm-resume
  "l"   'helm-for-files
  "M"   'helm-man-woman
  "f r" 'helm-recentf)

(define-key helm-map            (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map            (kbd "C-,")   #'helm-select-action)
(define-key helm-map            (kbd "C-j")   #'helm-next-line)
(define-key helm-map            (kbd "C-k")   #'helm-previous-line)
(define-key helm-map            (kbd "M-j")   #'helm-next-source)
(define-key helm-map            (kbd "M-k")   #'helm-previous-source)
(define-key helm-find-files-map (kbd "C-d")   #'helm-ff-persistent-delete)
(define-key helm-buffer-map     (kbd "C-d")   #'helm-buffer-run-kill-persistent)
(global-set-key                 (kbd "M-x")   #'helm-M-x)
(global-set-key                 (kbd "C-x b") #'helm-for-files)

(define-key helm-map [escape] #'helm-keyboard-quit)

(a/def-key-for-maps (kbd "C-p") #'helm-show-kill-ring default-mode-maps)

(provide 'helm-cfg)
;; helm-cfg.el ends here
