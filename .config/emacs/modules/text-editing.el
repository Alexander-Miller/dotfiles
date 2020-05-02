;; -*- lexical-binding: t -*-

(std::using-packages
 anzu
 evil
 evil-goggles
 evil-collection
 evil-surround
 evil-nerd-commenter
 evil-lion
 evil-exchange
 smartparens
 yasnippet
 (i3wm-config-mode :type git :host github :repo "Alexander-Miller/i3wm-config-mode")
 (tridactylrc-mode :type git :host github :repo "Alexander-Miller/tridactylrc-mode")
 expand-region
 vimish-fold
 evil-vimish-fold
 yaml-mode)

(autoload #'yas-expand "yasnippet")
(autoload #'evilnc-comment-operator "evil-nerd-commenter")

(std::autoload text-editing
  #'std::edit::fill-dwim
  #'std::edit::fold-defun
  #'std::edit::fold-list
  #'std::edit::defun-query-replace)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; General
(setq-default
 tab-width        4
 indent-tabs-mode nil)
(setf
 isearch-forward         t
 evil-search-module      'evil-search
 evil-want-C-i-jump      t
 evil-want-fine-undo     t
 evil-want-keybinding    t
 evil-want-Y-yank-to-eol t
 evil-move-beyond-eol    t
 next-line-add-newlines  t)

(evil-mode)
(evil-goggles-mode)
(evil-lion-mode)
(global-evil-surround-mode)
(global-subword-mode t)

(evil-set-initial-state 'messages-buffer-mode 'motion)
(evil-set-leader '(normal visual motion) (kbd "<SPC>"))

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

;; Custom Operators & Text Objects
(evil-define-operator std::evil::join (beg end)
  "Join the selected lines and indent."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (join-line 1)))
  (save-excursion
    (forward-line 1)
    (indent-for-tab-command)))

(evil-define-motion std::evil::forward-five-lines ()
  "Move the cursor 5 lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move 5)))

(evil-define-motion std::evil::backward-five-lines ()
  "Move the cursor 5 lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move -5)))

(evil-define-text-object std::evil::defun-object (count &optional beg end type)
  "Evil defun text object."
  (let ((start (point))
        (beg)
        (end))
    (mark-defun)
    (forward-line 1)
    (setf beg (region-beginning)
          end (region-end))
    (deactivate-mark)
    (goto-char start)
    (evil-range beg end type)))

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
(defun std::indent-after-paste-advice (yank-func &rest args)
  "If current mode is not one of spacemacs-indent-sensitive-modes
indent yanked text (with universal arg don't indent)."
  (evil-start-undo-step)
  (prog1
      (let ((prefix (car args))
            (enable (and (not (member major-mode '(python-mode)))
                         (derived-mode-p 'prog-mode))))
        (prog1 (apply yank-func args)
          (when enable
            (let ((transient-mark-mode nil)
                  (save-undo buffer-undo-list)
                  (beg (region-beginning))
                  (end (region-end)))
              (when (<= (- end beg) 5000)
                  (indent-region beg end nil))))))
    (evil-end-undo-step)))

(std::advice-add #'std::indent-after-paste-advice :around
  (yank yank-pop evil-paste-before evil-paste-after))

;; Snippets
(std::after yasnippet
  (add-hook 'snippet-mode-hook #'whitespace-mode)
  (setf yas-verbosity 3
        yas-snippet-dirs (list (concat std::emacs-dir "snippets")))
  (yas-reload-all)
  (yas-global-mode))

;; Folding
(std::schedule 1 :no-repeat
  (global-evil-vimish-fold-mode))

(setf
 evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))

;; Distinguish C-i & TAB
(define-key input-decode-map "\C-i" [C-i])

;; Keybinds
(std::keybind
 :global
 "M-q" #'std::edit::fill-dwim
 "C-7" #'evilnc-comment-operator
 :keymap evil-operator-state-map
 "üf" #'std::evil::defun-object
 :keymap (evil-normal-state-map evil-visual-state-map evil-motion-state-map)
 "<C-i>" #'evil-jump-forward
 "J"     #'std::evil::forward-five-lines
 "K"     #'std::evil::backward-five-lines
 "M"     #'evil-goto-mark-line
 "zD"    #'std::edit::fold-defun
 "zL"    #'std::edit::fold-list
 "zy"    #'vimish-fold-avy
 :keymap (evil-motion-state-map evil-normal-state-map evil-visual-state-map evil-insert-state-map)
 "TAB" #'indent-for-tab-command
 "C-e" #'evil-end-of-visual-line
 "C-a" #'evil-beginning-of-visual-line
 :keymap evil-normal-state-map
 "C-j"   #'newline-and-indent
 "C-S-j" #'std::evil::join
 "gx"    #'evil-exchange
 "gX"    #'evil-exchange-cancel
 :keymap evil-insert-state-map
 "C-l" #'yas-expand
 :leader
 "üü" #'anzu-query-replace
 "üf" #'std::edit::defun-query-replace
 "v"  #'er/expand-region)
