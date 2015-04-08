;; ========================
;; company auto completion
;; ========================

(global-company-mode t)

(add-to-list 'company-backends '(company-latex-commands company-math-symbols-latex company-math-symbols-unicode))

;;(company-auctex-init)

(setq company-idle-delay 3)

(define-key evil-normal-state-map (kbd "C-<SPC>") 'helm-company)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'helm-company)
(define-key company-active-map    (kbd "C-j")     'company-select-next)
(define-key company-active-map    (kbd "C-k")     'company-select-previous)
