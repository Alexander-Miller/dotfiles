;;; keybinds-cfg.el --- key bindings config

;;; Commentary:
;;; Code:

(evil-leader/set-key
  "f s"   #'save-buffer
  "f S"   #'save-some-buffers
  "f e"   #'eval-buffer
  "f w"   #'ace-swap-window
  "f k"   #'a/kill-delete-buffer
  "f K"   #'kill-buffer
  "f C-k" #'ace-delete-window
  "t h"   #'highlight-symbol
  "t s"   #'flyspell-mode
  "t S"   #'flyspell-buffer
  "t d"   #'a/choose-dict
  "t R"   #'a/reload-config
  "t r"   #'a/reload-config-file
  "C-j"   #'evil-join
  "o"     #'ace-window
  "O"     #'other-frame
  "0"     #'delete-window
  "1"     #'delete-other-windows
  "2"     #'split-window-vertically
  "3"     #'split-window-horizontally
  "r"     #'anzu-query-replace-regexp
  "R"     #'anzu-query-replace-at-cursor
  "q f"   #'vimish-fold
  "q d"   #'vimish-fold-delete
  "q q"   #'a/vimish-fold-dwim
  "q a"   #'vimish-fold-avy
  "q r"   #'vimish-refold
  "q A"   #'vimish-fold-unfold-all
  "q D"   #'vimish-fold-delete-all
  "q R"   #'vimish-fold-refold-all
  "e i"   #'el-get-install
  "e I"   #'el-get-reinstall
  "e r"   #'el-get-remove
  "e u"   #'el-get-update
  "e U"   #'el-get-self-update
  "e a"   #'el-get-update-all
  "e d"   #'el-get-describe)

(a/def-key-for-maps
 (kbd "j") #'evil-next-visual-line (remove evil-insert-state-map default-mode-maps))
(a/def-key-for-maps
 (kbd "k") #'evil-previous-visual-line (remove evil-insert-state-map default-mode-maps))

(a/def-key-for-maps
 (kbd "J") #'a/quick-forward
 (remove evil-insert-state-map default-mode-maps))
(a/def-key-for-maps
 (kbd "K") #'a/quick-backward
 (remove evil-insert-state-map default-mode-maps))

(a/def-key-for-maps
 (kbd "C-a") #'evil-beginning-of-visual-line default-mode-maps)
(a/def-key-for-maps
 (kbd "C-e") #'evil-end-of-visual-line default-mode-maps)
(define-key evil-visual-state-map (kbd "C-e")
 (lambda () (interactive) (evil-end-of-visual-line)))

(a/def-key-for-maps
 (kbd "C-s") #'evil-search-forward default-mode-maps)
(a/def-key-for-maps
 (kbd "C-r") #'evil-search-backward default-mode-maps)

(global-set-key (kbd "C-x C-e") '(lambda () (interactive) (a/eval-last-sexp 'eval-last-sexp)))

(a/def-key-for-maps
 (kbd "C-x x") 'eval-defun default-mode-maps)

(a/def-key-for-maps
 (kbd "C-7") 'comment-dwim-2 default-mode-maps)
(define-key evil-visual-state-map (kbd "C-7") 'comment-dwim-2)

(a/def-key-for-maps
 (kbd "C-x C-x") 'evil-goto-mark default-mode-maps)

(a/def-key-for-maps
 (kbd ",") 'evilmi-jump-items default-mode-maps)

(global-set-key (kbd "C-^") 'evil-buffer)

(global-set-key (kbd "C-c ö") 'a/what-face)

(define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)

(evil-define-key 'normal emacs-lisp-mode-map (kbd "M-.") #'xref-find-definitions)
(define-key evil-normal-state-map (kbd "M-,") #'xref-pop-marker-stack)

(eyebrowse-setup-opinionated-keys)

(define-key evil-normal-state-map (kbd "C-w") #'er/expand-region)

(global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt)

(define-key evil-visual-state-map (kbd "gx") #'evil-exchange)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB")   nil)
(define-key yas-minor-mode-map (kbd "C-ä")   #'yas-expand)
(evil-define-key 'normal prog-mode-map (kbd "TAB") #'indent-for-tab-command)

(provide 'keybinds-cfg)
;;; keybinds-cfg.el ends here
