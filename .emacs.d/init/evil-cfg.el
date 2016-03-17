;;; evil-cfg.el --- evil config

;;; Commentary:
;;; Code:

(if (not (bound-and-true-p global-evil-leader-mode))
    (evil-leader/set-leader "<SPC>"))

(global-evil-leader-mode 1)
(evil-mode 1)
(global-evil-matchit-mode 1)

(defvar default-mode-maps
  (list evil-normal-state-map evil-insert-state-map evil-visual-state-map evil-operator-state-map evil-motion-state-map)
  "List of evil's default keymaps that are relevant for remapping.")

(setq-default
 evil-auto-indent t)

(setq
 evil-default-state         'normal
 evil-move-cursor-back      t
 evil-move-beyond-eol       t
 evil-repeat-move-cursor    t
 evil-shift-width           4
 evil-want-fine-undo        'fine
 evil-normal-state-cursor   '("#ab3737" box)
 evil-insert-state-cursor   '("#33aa33" bar)
 evil-visual-state-cursor   '("#a374a8" box)
 evil-motion-state-cursor   '("#c97449" box)
 evil-operator-state-cursor '("#00688b" (hbar . 5))
 evil-emacs-state-cursor    '("#339999" bar)
 evil-resize-state-cursor   '("#ffdb1a" box))

(evil-set-initial-state 'special-mode 'motion)
(evil-set-initial-state 'messages-buffer-mode 'motion)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(evil-define-state resize
  "Evil Resize State"
  :tag "<R>"
  :suppress-keymap t)

(global-set-key (kbd "M-r") #'evil-resize-state)
(define-key evil-resize-state-map (kbd "ESC") #'evil-change-to-previous-state)
(define-key evil-resize-state-map (kbd "C-g") #'evil-change-to-previous-state)
(define-key evil-resize-state-map (kbd "j")   #'shrink-window)
(define-key evil-resize-state-map (kbd "k")   #'enlarge-window)
(define-key evil-resize-state-map (kbd "l")   #'shrink-window-horizontally)
(define-key evil-resize-state-map (kbd "h")   #'enlarge-window-horizontally)
(define-key evil-resize-state-map (kbd "J")   #'(lambda () (interactive) (shrink-window 5)))
(define-key evil-resize-state-map (kbd "K")   #'(lambda () (interactive) (enlarge-window 5)))
(define-key evil-resize-state-map (kbd "L")   #'(lambda () (interactive) (shrink-window-horizontally 5)))
(define-key evil-resize-state-map (kbd "H")   #'(lambda () (interactive) (enlarge-window-horizontally 5)))

(require 'volume)
(evil-define-state volume
  "Evil Volume State"
  :tag "<Vol>"
  :cursor ("#446677" hollow)
  :suppress-keymap t)

(global-set-key (kbd "M-v") #'evil-volume-state)
(define-key evil-volume-state-map (kbd "ESC") #'evil-change-to-previous-state)
(define-key evil-volume-state-map (kbd "C-g") #'evil-change-to-previous-state)
(define-key evil-volume-state-map (kbd "j")   #'volume-lower)
(define-key evil-volume-state-map (kbd "k")   #'volume-raise)
(define-key evil-volume-state-map (kbd "J")   #'volume-lower-10)
(define-key evil-volume-state-map (kbd "K")   #'volume-raise-10)

(provide 'evil-cfg)
;;; evil-cfg.el ends here
