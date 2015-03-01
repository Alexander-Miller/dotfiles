;; ========================
;; everything helm related
;; ========================

(helm-mode 1)

;; fuzzy matching wherever possible
(setq helm-M-x-fuzzy-match 1)
(setq helm-semantic-fuzzy-match 1)
(setq helm-imenu-fuzzy-match 1)
(setq helm-apropos-fuzzy-match 1)

;; cycle through selection
(setq helm-move-to-line-cycle-in-source 1)

;; search for library in require and declare-function sexp
(setq helm-ff-search-library-in-sexp 1)

;; scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount 8)

;; use recent file history
(setq helm-ff-file-name-history-use-recentf 1)

;; helm always covers half the buffer
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 50)
(setq helm-autoresize-min-height 50)

;; size of helm's buffers is not managed by golden ratio
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; keys
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-,") 'helm-select-action)

(global-set-key (kbd "M-x") 'helm-M-x)



