;; ============================
;; org-mode and its eco system
;; ============================

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(with-eval-after-load "org"
  (setq org-startup-indented 1)
  (setq org-startup-align-all-tables 1)
  (setq org-startup-folded 1)
  
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)

  (define-key org-mode-map (kbd "M-J") 'org-shiftmetadown)
  (define-key org-mode-map (kbd "M-K") 'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-H") 'org-shiftmetaleft)
  (define-key org-mode-map (kbd "M-L") 'org-shiftmetaright)

  (define-key org-mode-map (kbd "M-m") 'org-mark-element)

  (evil-leader/set-key-for-mode 'org-mode
    "<SPC> t r" 'org-table-recalculate-buffer-tables
    "<SPC> t R" 'org-table-recalculate
    "<SPC> t c" 'org-table-create-or-convert-from-region))
