;;; org-mode-cfg.el --- org-mode config

;;; Commentary:
;;; Code:

(defun a/org-mode-hook ()
  "Org-mode hook."
  (setq-local company-backends '((company-files company-dabbrev company-yasnippet)))
  (org-bullets-mode t))

(add-hook 'org-mode-hook #'a/org-mode-hook)

(with-eval-after-load "org"

  (require 'ox-md nil t)
  (require 'ox-confluence nil t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (shell      . t)))

  (setq
   org-catch-invisible-edits      'show
   org-fontify-whole-heading-line nil
   org-list-indent-offset         1
   org-special-ctrl-a             nil
   org-special-ctrl-k             nil
   org-src-fontify-natively       t
   org-startup-align-all-tables   t
   org-startup-folded             t
   org-startup-indented           t)

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

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
    "SPC"       #'org-ctrl-c-ctrl-c
    "h i"       #'helm-org-in-buffer-headings
    "<tab> o"   #'org-todo
    "<tab> C-o" #'org-insert-todo-heading-respect-content
    "<tab> c"   #'org-toggle-checkbox
    "<tab> -"   #'org-ctrl-c-minus
    "<tab> u"   #'outline-up-heading
    "<tab> j"   #'org-forward-heading-same-level
    "<tab> k"   #'org-backward-heading-same-level
    "<tab> w"   #'org-refile
    "<tab> ="   #'org-sort
    "<tab> p s" #'org-set-property
    "<tab> n n" #'narrow-to-region
    "<tab> n b" #'org-narrow-to-block
    "<tab> n e" #'org-narrow-to-element
    "<tab> n s" #'org-narrow-to-subtree
    "<tab> n w" #'widen
    "<tab> s m" #'org-mark-subtree
    "<tab> s d" #'org-cut-subtree
    "<tab> s y" #'org-copy-subtree
    "<tab> s p" #'org-paste-subtree
    "<tab> t t" #'org-ctrl-c-star
    "<tab> t r" #'org-table-recalculate-buffer-tables
    "<tab> t R" #'org-table-recalculate
    "<tab> t c" #'org-table-create-or-convert-from-region))

(provide 'org-mode-cfg)
;;; org-mode-cfg.el ends here
