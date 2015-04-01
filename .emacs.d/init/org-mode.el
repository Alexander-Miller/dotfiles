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
    "<tab> o"   'org-todo
    "<tab> C-o" 'org-insert-todo-heading-respect-content
    "<tab> -"   'org-ctrl-c-minus
    "<tab> u"   'outline-up-heading
    "<tab> j"   'org-forward-heading-same-level
    "<tab> k"   'org-backward-heading-same-level
    "<tab> w"   'org-refile
    "<tab> s m" 'org-mark-subtree
    "<tab> s x" 'org-cut-subtree
    "<tab> s c" 'org-copy-subtree
    "<tab> s p" 'org-paste-subtree
    "<tab> t r" 'org-table-recalculate-buffer-tables
    "<tab> t R" 'org-table-recalculate
    "<tab> t c" 'org-table-create-or-convert-from-region))
