;;; evil-cfg.el --- evil config

;;; Commentary:
;;; Code:

(if (not (bound-and-true-p global-evil-leader-mode))
    (evil-leader/set-leader "<SPC>"))

(global-evil-leader-mode 1)
(evil-mode 1)
(global-evil-matchit-mode 1)

(setq-default
 evil-auto-indent           0
 evil-default-state         'normal
 evil-find-skip-newlines    t
 evil-move-cursor-back      t
 evil-move-beyond-eol       t
 evil-repeat-move-cursor    0
 evil-shift-width           4
 evil-want-fine-undo        t
 evil-normal-state-cursor   '("#ab3737" box)
 evil-insert-state-cursor   '("#33aa33" bar)
 evil-motion-state-cursor   '("#c97449" box)
 evil-operator-state-cursor '("#cc0000" (hbar . 5))
 evil-emacs-state-cursor    '("#339999" bar)
 evil-resize-state-cursor   '("#ffdb1a" box))

(evil-set-initial-state 'special-mode 'motion)

(evil-define-state resize
  "Evil Resize State"
  :tag "Resize"
  :suppress-keymap t)

(define-key evil-normal-state-map (kbd "M-r") #'evil-resize-state)
(define-key evil-resize-state-map (kbd "ESC") #'evil-normal-state)
(define-key evil-resize-state-map (kbd "C-g") #'evil-normal-state)
(define-key evil-resize-state-map (kbd "j")   #'shrink-window)
(define-key evil-resize-state-map (kbd "k")   #'enlarge-window)
(define-key evil-resize-state-map (kbd "l")   #'shrink-window-horizontally)
(define-key evil-resize-state-map (kbd "h")   #'enlarge-window-horizontally)
(define-key evil-resize-state-map (kbd "J")   #'(lambda () (interactive) (shrink-window 5)))
(define-key evil-resize-state-map (kbd "K")   #'(lambda () (interactive) (enlarge-window 5)))
(define-key evil-resize-state-map (kbd "L")   #'(lambda () (interactive) (shrink-window-horizontally 5)))
(define-key evil-resize-state-map (kbd "H")   #'(lambda () (interactive) (enlarge-window-horizontally 5)))

(provide 'evil-cfg)
;;; evil-cfg.el ends here
