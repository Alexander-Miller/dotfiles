;; ========================
;; LaTeX and its eco system
;; ========================

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

;;activate reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX 1)

(add-hook 'LaTeX-mode-hook 'latex-math-mode)

;;proper indentation of list items
(setq LaTeX-item-indent 0)

;; dont use different text sizes
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)

(evil-leader/set-key-for-mode 'latex-mode
  "<tab> e"   'LaTeX-environment
  "<tab> s"   'LaTeX-section
  "<tab> j"   'LaTeX-insert-item
  "<tab> ]"   'LaTeX-close-environment
  "<tab> f e" 'LaTeX-fill-environment
  "<tab> f r" 'LaTeX-fill-region
  "<tab> f s" 'LaTeX-fill-section
  "<tab> f f" 'LaTeX-fill-buffer
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

