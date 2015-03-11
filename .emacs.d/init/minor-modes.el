;; =========================================
;; permanent minor modes and their settings
;; =========================================

;; immediately show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; cursor blinks forever
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

;; parse buffers for their semantic content
(semantic-mode 1)

;; highlight marked regions
(transient-mark-mode 1)

;; hide unnecessary bars and menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; save sessions when emacs quits
(desktop-save-mode 1)

;; forward-word in camelCase words
(subword-mode 1)

;; always show line numbers
(global-nlinum-mode 1)

;; autocomplete parens, quotation marks etc
(electric-pair-mode 1)

;; automatic indentation
;;(aggressive-indent-global-mode 1)

;; highlight lines longer than 120 characters
(setq whitespace-line-column 120)

;; show proper undo tree
(undo-tree-mode 1)

;; automatic resizing of focussed buffers
(golden-ratio-mode 1)

;; buffers not managed by golden ratio
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
(add-to-list 'golden-ratio-exclude-modes "neotree-mode")

;; turn symbols into their unicode counterparts
(global-prettify-symbols-mode 1)

;; navigation based on visual, not logical lines
(global-visual-line-mode 1)

(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'automatic)

