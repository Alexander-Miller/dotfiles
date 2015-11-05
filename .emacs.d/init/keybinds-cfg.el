;;; keybinds-cfg.el --- key bindings config

;;; Commentary:
;;; Code:

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; beginning and end of line
(my/def-key-for-maps
 (kbd "C-a") 'evil-beginning-of-visual-line
 (list evil-normal-state-map evil-insert-state-map evil-visual-state-map evil-operator-state-map))
(my/def-key-for-maps
 (kbd "C-e") 'evil-end-of-visual-line
 (list evil-normal-state-map evil-insert-state-map evil-visual-state-map evil-operator-state-map))

;; next and previous line -> visual
(my/def-key-for-maps
 (kbd "j") 'evil-next-visual-line
 (list evil-normal-state-map evil-visual-state-map evil-operator-state-map))
(my/def-key-for-maps
 (kbd "k") 'evil-previous-visual-line
 (list evil-normal-state-map evil-visual-state-map evil-operator-state-map))

;; word and character searching
(my/def-key-for-maps
 (kbd "C-s") 'evil-search-forward
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map evil-motion-state-map))
(my/def-key-for-maps
 (kbd "C-r") 'evil-search-backward
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map evil-motion-state-map evil-motion-state-map))
(my/def-key-for-maps
 (kbd "M-p") 'evil-avy-goto-char
 (list evil-normal-state-map evil-visual-state-map evil-operator-state-map evil-motion-state-map))

;; faster scrolling
(my/def-key-for-maps
 (kbd "J") 'my/quick-forward
 (list evil-normal-state-map evil-motion-state-map evil-operator-state-map evil-visual-state-map))
(my/def-key-for-maps
 (kbd "K") 'my/quick-backward
 (list evil-normal-state-map evil-motion-state-map evil-operator-state-map evil-visual-state-map))

;; jump paren pairs with ,
(my/def-key-for-maps
 (kbd ",") 'evilmi-jump-items
 (list evil-normal-state-map evil-visual-state-map evil-operator-state-map evil-motion-state-map))

;; screen recenter
(my/def-key-for-maps
 (kbd "C-x l") 'recenter-top-bottom
 (list evil-normal-state-map evil-insert-state-map evil-visual-state-map evil-motion-state-map evil-emacs-state-map))

;; evaluate definition at point
(my/def-key-for-maps
 (kbd "C-x x") 'eval-defun
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map))
(global-set-key (kbd "C-x C-e") '(lambda () (interactive) (my/eval-last-sexp 'eval-last-sexp)))

;; inserting newlines
(my/def-key-for-maps
 (kbd "C-j") 'my/newline-and-indent
 (list evil-normal-state-map evil-insert-state-map))

;; adding/removing comments
(my/def-key-for-maps
 (kbd "C-7") 'comment-line
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map))
(define-key evil-visual-state-map (kbd "C-7") 'comment-dwim)

;; escape quits everything
(my/def-key-for-maps
 [escape] 'keyboard-quit
 (list evil-normal-state-map evil-operator-state-map evil-visual-state-map evil-emacs-state-map))
(define-key helm-map           [escape] 'helm-keyboard-quit)
(define-key company-active-map [escape] 'company-abort)

;; better pasting
(my/def-key-for-maps
 (kbd "C-p") 'helm-show-kill-ring
 (list evil-normal-state-map evil-insert-state-map evil-normal-state-map))

;; goto mark
(my/def-key-for-maps
 (kbd "C-x C-x") 'evil-goto-mark
 (list evil-normal-state-map evil-insert-state-map evil-operator-state-map))

;; expand region
(define-key evil-normal-state-map (kbd "C-w") 'er/expand-region)

;; for convenience
(global-set-key (kbd "C-c ö") 'my/what-face)
(global-set-key (kbd "C-^") 'evil-buffer)

;; evil numbers
(global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt)

;; evil exchange
(my/def-key-for-maps
 (kbd "gx") 'evil-exchange
 (list evil-normal-state-map evil-visual-state-map))

;; redo
(define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)

;; tab indent in normal mode
(define-key prog-mode-map (kbd "<tab>") 'indent-for-tab-command)

;; basic jump to definition
(define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
(define-key evil-normal-state-map (kbd "M-,") #'xref-pop-marker-stack)

;; company binds
(my/def-key-for-maps
 (kbd "C-<SPC>") 'company-complete
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map))
(define-key company-active-map (kbd "C-j")   'company-select-next)
(define-key company-active-map (kbd "C-k")   'company-select-previous)
(define-key company-active-map (kbd "C-ü")   'helm-company)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "C-o")   'company-other-backend)

;; helm binds
(define-key helm-map            (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map            (kbd "C-,")   'helm-select-action)
(define-key helm-map            (kbd "C-j")   'helm-next-line)
(define-key helm-map            (kbd "C-k")   'helm-previous-line)
(define-key helm-map            (kbd "M-j")   'helm-next-source)
(define-key helm-map            (kbd "M-k")   'helm-previous-source)
(define-key helm-find-files-map (kbd "C-d")   'helm-ff-persistent-delete)
(define-key helm-buffer-map     (kbd "C-d")   'helm-buffer-run-kill-persistent)
(global-set-key                 (kbd "M-x")   'helm-M-x)
(global-set-key                 (kbd "C-x b") 'my/helm-mini-below)

;; yasnippet binds
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-ä") 'yas-expand)

(evil-leader/set-key
  "f s" 'save-buffer
  "f f" 'helm-find-files
  "f S" 'save-some-buffers
  "f e" 'eval-buffer
  "f k" 'kill-this-buffer
  "f K" 'kill-buffer
  "f r" 'helm-recentf
  "t h" 'highlight-symbol
  "t v" 'volume
  "H H" 'helm-apropos
  "g s" 'magit-status
  "h i" 'helm-semantic-or-imenu
  "h s" 'helm-swoop
  "h S" 'helm-multi-swoop
  "h a" 'helm-ag
  "h A" 'helm-do-ag
  "h r" 'helm-resume
  "l"   'my/helm-mini-below
  "M"   'helm-man-woman
  "C-j" 'evil-join
  "o"   'ace-window
  "O"   'other-frame
  "0"   'delete-window
  "1"   'delete-other-windows
  "2"   'split-window-vertically
  "3"   'split-window-horizontally
  "r"   'anzu-query-replace-regexp
  "R"   'anzu-query-replace-at-cursor
  "+"   'set-mark-command
  "j"   'avy-goto-line
  "q f" 'vimish-fold
  "q d" 'vimish-fold-delete
  "q q" 'vimish-fold-toggle
  "q a" 'vimish-fold-avy
  "q r" 'vimish-refold
  "e i" 'el-get-install
  "e I" 'el-get-reinstall
  "e r" 'el-get-remove
  "e u" 'el-get-update
  "e U" 'el-get-self-update
  "e a" 'el-get-update-all
  "e d" 'el-get-describe)

(provide 'keybinds-cfg)
;;; keybinds-cfg.el ends here
