;;; latex-cfg.el --- latex eco-system configuration

;;; Commentary:

;;; Code:

;; latex mode hooks
(defun latex-company-setup ()
  "Use only relevant backends."
  (setq-local company-backends
              '((company-latex-commands
                 company-math-symbols-latex
                 company-math-symbols-unicode
                 company-auctex-labels
                 company-auctex-bibs
                 company-auctex-macros
                 company-auctex-symbols
                 company-auctex-environments
                 company-yasnippet
                 company-files
                 company-dabbrev))))

(add-hook 'TeX-mode-hook
          '(lambda ()
             (rainbow-delimiters-mode t)
             (turn-on-reftex)
             (setq reftex-plug-into-AUCTeX t)
             (latex-math-mode t)
             (tex-fold-mode t)
             (latex-company-setup)))

(with-eval-after-load "latex"

  (setq-default
   font-latex-fontify-script     nil
   font-latex-fontify-sectioning 'color
   LaTeX-item-indent             0
   TeX-master                    nil
   TeX-save-query                nil
   TeX-auto-save                 t
   TeX-parse-self                t
   TeX-PDF-mode                  t)

  (evil-leader/set-key-for-mode 'latex-mode
    "<tab> e"   'LaTeX-environment
    "<tab> s"   'LaTeX-section
    "<tab> i"   'LaTeX-insert-item
    "<tab> c"   'LaTeX-close-environment
    "<tab> m"   'TeX-insert-macro

    "<tab> f e" 'LaTeX-fill-environment
    "<tab> f r" 'LaTeX-fill-region
    "<tab> f s" 'LaTeX-fill-section
    "<tab> f f" 'LaTeX-fill-buffer

    "<tab> l b" 'TeX-fold-buffer
    "<tab> l c" 'TeX-fold-comment
    "<tab> l l" 'TeX-fold-dwim
    "<tab> l f" 'TeX-fold-buffer
    "<tab> l e" 'TeX-fold-env
    "<tab> l r" 'TeX-fold-region
    "<tab> L i" 'TeX-fold-clearout-item
    "<tab> L f" 'TeX-fold-clearout-buffer
    "<tab> L p" 'TeX-fold-clearout-paragraph
    "<tab> L r" 'TeX-fold-clearout-region

    "<tab> o b" '(lambda () (interactive) (TeX-font nil 2)  (evil-insert 1))
    "<tab> o s" '(lambda () (interactive) (TeX-font nil 3)  (evil-insert 1))
    "<tab> o e" '(lambda () (interactive) (TeX-font nil 5)  (evil-insert 1))
    "<tab> o i" '(lambda () (interactive) (TeX-font nil 9)  (evil-insert 1))
    "<tab> o r" '(lambda () (interactive) (TeX-font nil 18) (evil-insert 1))
    "<tab> o l" '(lambda () (interactive) (TeX-font nil 19) (evil-insert 1))
    "<tab> o t" '(lambda () (interactive) (TeX-font nil 20) (evil-insert 1))
    "<tab> o d" '(lambda () (interactive) (TeX-font nil 4))

    "<tab> p e" 'preview-environment
    "<tab> p r" 'preview-region
    "<tab> p s" 'preview-section
    "<tab> p f" 'preview-buffer
    "<tab> p p" 'preview-at-point
    "<tab> P r" 'preview-clearout
    "<tab> P s" 'preview-clearout-section
    "<tab> P b" 'preview-clearout-buffer
    "<tab> P p" 'preview-clearout-at-point
    "<tab> P f" 'preview-clearout-document))

(provide 'latex-cfg)
;;; latex-cfg.el ends here
