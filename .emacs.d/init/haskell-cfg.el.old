;;; haskell-cfg.el --- haskell config

;;; Commentary:
;;; Code:

(defun haskell-hook ()
  "Haskell mode hook."
  (turn-on-haskell-indent)
  (setq-local company-backends '((company-ghc company-files company-keywords company-dabbrev-code company-yasnippet)))
  (ghc-init))

(add-hook 'haskell-mode-hook  #'haskell-hook)
(add-hook 'haskell-mode-hook  #'interactive-haskell-mode)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(with-eval-after-load "haskell"

  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))

  (setq-default
   company-ghc-show-info                 t
   company-ghc-show-module               t
   company-ghc-component-prefix-match    t
   haskell-process-type                  'cabal-repl
   haskell-indentation-show-indentations nil
   haskell-tags-on-save                  t)

  (evil-leader/set-key-for-mode 'haskell-mode
    "<tab> b" 'haskell-interactive-switch
    "<tab> l" 'haskell-process-load-file
    "<tab> t" 'haskell-process-do-type
    "<tab> i" 'haskell-process-do-info
    "<tab> f" 'haskell-mode-tag-find)

  (evil-leader/set-key-for-mode 'haskell-interactive-mode
    "<tab> b" 'haskell-interactive-switch-back)

  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c")   'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC")     'haskell-mode-contextual-space)

  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal))

(provide 'haskell-cfg)
;;; haskell-cfg.el ends here
