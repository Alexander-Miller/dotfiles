;; ========================
;; LaTeX and its eco system
;; ========================

;; latex mode hooks
(add-hook 'latex-mode-hook
          '(lambda ()
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
  "[TAB] e"   'LaTeX-environment
  "[TAB] s"   'LaTeX-section
  "[TAB] j"   'LaTeX-insert-item
  "[TAB] ]"   'LaTeX-close-environment
  "[TAB] f e" 'LaTeX-fill-environment
  "[TAB] f r" 'LaTeX-fill-region
  "[TAB] f s" 'LaTeX-fill-section
  "[TAB] f f" 'LaTeX-fill-buffer
  "[TAB] p e" 'preview-environment
  "[TAB] p r" 'preview-region
  "[TAB] p s" 'preview-section
  "[TAB] p f" 'preview-buffer
  "[TAB] p p" 'preview-at-point
  "[TAB] P r" 'preview-clearout
  "[TAB] P s" 'preview-clearout-section
  "[TAB] P b" 'preview-clearout-buffer
  "[TAB] P p" 'preview-clearout-at-point
  "[TAB] P f" 'preview-clearout-document)

