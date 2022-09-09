;; -*- lexical-binding: t -*-

(std::using-packages
 anzu
 evil
 evil-goggles
 evil-collection
 evil-surround
 evil-nerd-commenter
 evil-exchange
 (evil-numbers :type git :host github :repo "janpath/evil-numbers")
 (undo-tree :type git :host gitlab :repo "tsc25/undo-tree")
 smartparens
 yasnippet
 i3wm-config-mode
 (tridactylrc-mode :type git :host github :repo "Alexander-Miller/tridactylrc-mode")
 expand-region
 vimish-fold
 evil-vimish-fold
 adoc-mode
 yaml-mode
 csv-mode
 kotlin-mode
 groovy-mode
 markdown-mode)

(autoload #'yas-expand "yasnippet")
(autoload #'evilnc-comment-operator "evil-nerd-commenter")

(std::autoload text-editing
  #'std::edit::fill-dwim
  #'std::edit::fold-defun
  #'std::edit::fold-list
  #'std::edit::fold-hydra/body
  #'std::edit::defun-query-replace
  #'std::edit::evil-join
  #'std::edit::evil-forward-five-lines
  #'std::edit::evil-backward-five-lines
  #'std::edit::evil-defun-object
  #'std::edit::indent-after-paste-advice)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; General
(setq-default
 tab-width        4
 indent-tabs-mode nil)

(setf
 isearch-forward           t
 evil-search-module        'evil-search
 evil-want-C-i-jump        t
 evil-kill-on-visual-paste nil
 evil-want-fine-undo       t
 evil-want-keybinding      t
 evil-want-Y-yank-to-eol   t
 evil-move-beyond-eol      t
 next-line-add-newlines    t
 evil-undo-system          'undo-tree
 undo-tree-history-directory-alist
 `((".+" . "~/.emacs.d/cache")))

(std::pushnew auto-mode-alist
  '("\\.service\\'" . conf-mode)
  '("\\.timer\\'" . conf-mode))

(evil-mode)
(evil-goggles-mode)
(global-evil-surround-mode)
(global-subword-mode t)
(global-undo-tree-mode)

(std::keybind
 :keymap (evil-normal-state-map evil-motion-state-map)
 "SPC" std::leader-keymap)

(evil-set-initial-state 'xref--xref-buffer-mode 'motion)
(evil-set-initial-state 'messages-buffer-mode   'motion)
(evil-set-initial-state 'Custom-mode            'motion)
(evil-set-initial-state 'special-mode           'motion)
(evil-set-initial-state 'debugger-mode          'motion)

;; Cursors
(setf
 evil-normal-state-cursor   '("#ab3737" box)
 evil-insert-state-cursor   '("#33aa33" bar)
 evil-visual-state-cursor   '("#a374a8" box)
 evil-motion-state-cursor   '("#c97449" box)
 evil-operator-state-cursor '("#00688b" (hbar . 5))
 evil-emacs-state-cursor    '("#339999" bar))

;; Goggles
(setf
 evil-goggles-duration                     0.15
 evil-goggles-pulse                        nil
 evil-goggles-enable-change                t
 evil-goggles-enable-commentary            t
 evil-goggles-enable-delete                t
 evil-goggles-enable-fill-and-move         t
 evil-goggles-enable-indent                t
 evil-goggles-enable-join                  t
 evil-goggles-enable-nerd-commenter        t
 evil-goggles-enable-paste                 t
 evil-goggles-enable-record-macro          t
 evil-goggles-enable-redo                  t
 evil-goggles-enable-replace-with-register t
 evil-goggles-enable-set-marker            t
 evil-goggles-enable-shift                 t
 evil-goggles-enable-surround              t
 evil-goggles-enable-undo                  t
 evil-goggles-enable-yank                  t)

;; Expand Region
(std::after expand-region
  (setf expand-region-contract-fast-key "c"
        expand-region-reset-fast-key    "r"))

;; Smartparens
(std::schedule 1 :no-repeat
  (require 'smartparens-config)
  (smartparens-global-mode))

(std::after smartparens
  (show-paren-mode -1)
  (show-smartparens-global-mode)

  (setf
   sp-echo-match-when-invisible            nil
   sp-max-prefix-length                    25
   sp-show-pair-delay                      0.2
   sp-show-pair-from-inside                t
   sp-cancel-autoskip-on-backward-movement nil
   sp-highlight-pair-overlay               nil
   sp-highlight-wrap-overlay               nil
   sp-highlight-wrap-tag-overlay           nil)

  (std::keybind
   :leader
   "kr" #'sp-raise-sexp
   "kw" #'sp-wrap-round
   "ks" #'sp-forward-slurp-sexp
   "kS" #'sp-backward-slurp-sexp
   "kb" #'sp-forward-barf-sexp
   "kB" #'sp-backward-barf-sexp
   "js" #'sp-split-sexp
   "jn" #'sp-newline))

;; Paste-Indent

(std::add-advice #'std::edit::indent-after-paste-advice :around
  (yank yank-pop evil-paste-before evil-paste-after))

;; Snippets
(std::after yasnippet
  (add-hook 'snippet-mode-hook #'whitespace-mode)
  (setf yas-verbosity 3
        yas-snippet-dirs (list (concat std::dirs::emacs "snippets")))
  (yas-reload-all)
  (yas-global-mode))

;; Folding
(std::schedule 1 :no-repeat
  (global-evil-vimish-fold-mode)
  (setf
   evil-vimish-fold-target-modes
   '(prog-mode conf-mode text-mode)))

;; Distinguish C-i & TAB
(define-key input-decode-map "\C-i" [C-i])

(std::keybind
 :global
 "M-q"   #'std::edit::fill-dwim
 "C-7"   #'evilnc-comment-operator
 "C-c +" #'evil-numbers/inc-at-pt
 "C-c -" #'evil-numbers/dec-at-pt
 :keymap evil-operator-state-map
 "üf" #'std::edit::evil-defun-object
 :keymap (evil-normal-state-map evil-visual-state-map evil-motion-state-map)
 "<C-i>" #'evil-jump-forward
 "J"     #'std::edit::evil-forward-five-lines
 "K"     #'std::edit::evil-backward-five-lines
 "M"     #'evil-goto-mark-line
 "zD"    #'std::edit::fold-defun
 "zL"    #'std::edit::fold-list
 "zy"    #'vimish-fold-avy
 :keymap (evil-motion-state-map evil-normal-state-map evil-visual-state-map evil-insert-state-map)
 "TAB" #'indent-for-tab-command
 "C-e" #'evil-end-of-visual-line
 "C-a" #'evil-beginning-of-visual-line
 :keymap evil-normal-state-map
 "Q"     #'evil-execute-macro
 "C-j"   #'newline-and-indent
 "C-S-j" #'std::edit::evil-join
 "gx"    #'evil-exchange
 "gX"    #'evil-exchange-cancel
 :keymap evil-insert-state-map
 "C-l" #'yas-expand
 :leader
 "nd" #'narrow-to-defun
 "nr" #'narrow-to-region
 "nw" #'widen
 "Z"  #'std::edit::fold-hydra/body
 "üü" #'anzu-query-replace
 "üf" #'std::edit::defun-query-replace
 "v"  #'er/expand-region)
