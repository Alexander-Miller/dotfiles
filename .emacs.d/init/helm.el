;; ========================
;; everything helm related
;; ========================

(helm-mode 1)

;;helm variables
(setq
 helm-M-x-fuzzy-match                   t   ;; fuzzy matching wherever possible
 helm-semantic-fuzzy-match              t   ;; ~
 helm-imenu-fuzzy-match                 t   ;; ~
 helm-apropos-fuzzy-match               t   ;; ~
 helm-buffers-fuzzy-matching            t   ;; ~
 helm-completion-in-region-fuzzy-match  t   ;; ~
 helm-file-cache-fuzzy-match            t   ;; ~
 helm-projectile-fuzzy-match            t   ;; ~
 helm-semantic-fuzzy-match              t   ;; ~
 helm-locate-fuzzy-match                t   ;; ~
 helm-recentf-fuzzy-match               t   ;; ~
 helm-candidate-number-limit            100 ;; don't set too high or fuzzy matching will be slow
 helm-move-to-line-cycle-in-source      t   ;; cycle through selection
 helm-scroll-amount                     5   ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf  t   ;; use recent file history
 helm-autoresize-mode                   t   ;; let helm buffers always take half the screen
 helm-autoresize-max-height             50  ;; ~
 helm-autoresize-min-height             50  ;; ~
 helm-adaptive-mode                     t   ;; toggle adaptive sorting in all sources
 helm-ff-search-library-in-sexp         t   ;; search for library in require and declare-function sexp
 helm-quick-update                      t   ;; don't show out-of-screen sources to make helm faster
 helm-idle-delay                        0.1 ;; delay before helm updates in delayed sources
 helm-input-idle-delay                  0.1 ;; delay before helm udates - also in undelayed sources
 helm-display-source-at-screen-top      nil ;; don't display different sources right at the top
 helm-completion-window-scroll-margin   5   ;; leave small margin at the top and bottom
 helm-kill-ring-max-lines-number        5   ;; show at most 5 lines of kill ring candidates
 helm-default-external-file-browser     "urxvt -e ranger" ;; always use ranger
 helm-ff-transformer-show-only-basename t   ;; don't show full paths in helm-find-files
 helm-ff-auto-update-initial-value      nil ;; don't automatically switch with only one candidate or there's trouble making files
 )

;; remove unnecessary help header
(defadvice helm-display-mode-line (after undisplay-header activate)
  (setq header-line-format nil))

;; helm and global keys
(define-key helm-map            (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map            (kbd "C-,")   'helm-select-action)
(define-key helm-map            (kbd "C-j")   'helm-next-line)
(define-key helm-map            (kbd "C-k")   'helm-previous-line)
(define-key helm-find-files-map (kbd "C-d")   'helm-ff-persistent-delete)
(define-key helm-buffer-map     (kbd "C-d")   'helm-buffer-run-kill-persistent)
(global-set-key                 (kbd "M-x")   'helm-M-x)
(global-set-key                 (kbd "C-x b") 'helm-buffers-list)
