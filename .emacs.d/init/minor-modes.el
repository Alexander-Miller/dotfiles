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

;; reparse buffers when idle
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)

;; show semantic info about token at point
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

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

;; make sure golden ratio works with ace-window
(add-to-list 'golden-ratio-extra-commands 'ace-window)

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

;; mode line theming
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'automatic)

;; escreen init
(escreen-install)
(setq escreen-prefix-char (kbd "C-ü"))
(define-key escreen-map (kbd "d") 'escreen-goto-next-screen)
(define-key escreen-map (kbd "a") 'escreen-goto-prev-screen)
(define-key escreen-map (kbd "l") 'escreen-menu)

;; rainbow delimiter hooks
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; better line breaking
(global-visual-line-mode t)

;; must be loaded manually or std git-gutter is used
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; so we can switch to normal mode via jk
(key-chord-mode t)
