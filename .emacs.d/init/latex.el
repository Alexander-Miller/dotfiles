;; ========================
;; LaTeX and its eco system
;; ========================

;; latex mode hooks
(add-hook 'latex-mode-hook
          '(lambda ()
             '(rainbow-delimiters-mode)
             '(turn-on-reftex)
             '(setq reftex-plug-into-AUCTeX 1)
             '(latex-math-mode)))

;; save style information when saving the buffer
(setq TeX-auto-save 1)

;; parse file after loading if no style hook is found
(setq TeX-parse-self 1)

;;AUCTeX multidoc awareness for \input
(setq-default TeX-master nil)

;; use PDF TeX as default executable
(setq TeX-PDF-mode 1)

;; do not ask for permission to start TeX
(setq TeX-save-query nil)

;;proper indentation of list items
(setq LaTeX-item-indent 0)

;; dont use different text sizes
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)

(evil-leader/set-key-for-mode 'latex-mode
  "<tab> e"   'LaTeX-environment
  "<tab> s"   'LaTeX-section
  "<tab> i"   'LaTeX-insert-item
  "<tab> c"   'LaTeX-close-environment

  "<tab> f e" 'LaTeX-fill-environment
  "<tab> f r" 'LaTeX-fill-region
  "<tab> f s" 'LaTeX-fill-section
  "<tab> f f" 'LaTeX-fill-buffer

  "<tab> o b" '(lambda () (interactive) (TeX-font nil 2) (evil-insert 1))
  "<tab> o s" '(lambda () (interactive) (TeX-font nil 3) (evil-insert 1))
  "<tab> o e" '(lambda () (interactive) (TeX-font nil 5) (evil-insert 1))
  "<tab> o i" '(lambda () (interactive) (TeX-font nil 9) (evil-insert 1))
  "<tab> o r" '(lambda () (interactive) (TeX-font nil 18) (evil-insert 1))
  "<tab> o l" '(lambda () (interactive) (TeX-font nil 19) (evil-insert 1))
  "<tab> o t" '(lambda () (interactive) (TeX-font nil 20) (evil-insert 1))

  "<tab> p e" 'preview-environment
  "<tab> p r" 'preview-region
  "<tab> p s" 'preview-section
  "<tab> p f" 'preview-buffer
  "<tab> p p" 'preview-at-point
  "<tab> P r" 'preview-clearout
  "<tab> P s" 'preview-clearout-section
  "<tab> P b" 'preview-clearout-buffer
  "<tab> P p" 'preview-clearout-at-point
  "<tab> P f" 'preview-clearout-document)

