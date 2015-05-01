;; ========================
;; evil and its eco system
;; ========================

;; the key will no longer work after this file was reloaded, so
;; it must only be set once
(if (not (bound-and-true-p global-evil-leader-mode))
    (evil-leader/set-leader "<SPC>"))

(global-evil-leader-mode 1)
(evil-mode 1)
(global-evil-surround-mode 1)
(global-evil-matchit-mode 1)

(setq evil-default-state 'normal)

;; not needed with aggresive-indent-mode
(setq evil-auto-indent 0)

;; shift width via <>
(setq evil-shift-width 4)

;; repeating with . will not move the cursor
(setq evil-repeat-move-cursor 0)

;; character search will skip newlines
(setq evil-find-skip-newlines t)

;; better than escape
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
(define-key evil-normal-state-map (kbd "C-s") 'evil-search-forward)
(define-key evil-normal-state-map (kbd "C-r") 'evil-search-backward)
(my/def-key-for-maps
 (kbd "C-f") 'ace-jump-char-mode
 (list evil-normal-state-map evil-visual-state-map evil-operator-state-map))

;; faster scrolling
(my/def-key-for-maps
 (kbd "J") 'my/quick-forward
 (list evil-normal-state-map evil-motion-state-map))
(my/def-key-for-maps
 (kbd "K") 'my/quick-backward
 (list evil-normal-state-map evil-motion-state-map))

;; jump paren pairs with ,
(my/def-key-for-maps
 (kbd ",") 'evilmi-jump-items
 (list evil-normal-state-map evil-visual-state-map evil-motion-state-map))

;; recenter screen in all states
(my/def-key-for-maps
 (kbd "C-x l") 'recenter-top-bottom
 (list evil-normal-state-map evil-insert-state-map evil-visual-state-map evil-motion-state-map evil-emacs-state-map))

;; evaluate definition at point
(my/def-key-for-maps
 (kbd "C-x x") 'eval-defun
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map))

;; inserting newlines
(my/def-key-for-maps
 (kbd "C-j") 'my/newline-and-indent
 (list evil-normal-state-map evil-insert-state-map))

;; adding/removing comments
(my/def-key-for-maps
 (kbd "C-7") 'comment-line
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map))

;; escape quits everything
(my/def-key-for-maps
 [escape] 'keyboard-quit
 (list evil-normal-state-map evil-operator-state-map evil-visual-state-map evil-emacs-state-map))
(define-key helm-map [escape] 'keyboard-quit)

(my/def-key-for-maps
 (kbd "C-x C-x") 'evil-goto-mark
 (list evil-normal-state-map evil-insert-state-map evil-operator-state-map))

(my/def-key-for-maps
 (kbd "C-p") 'helm-show-kill-ring
 (list evil-normal-state-map evil-insert-state-map evil-normal-state-map))

(define-key evil-emacs-state-map (kbd "C-v") 'set-mark-command)

(evil-leader/set-key
  "f s" 'save-buffer
  "f o" 'helm-find-files
  "f S" 'save-some-buffers
  "f e" 'eval-buffer
  "f k" 'kill-buffer
  "f r" 'helm-recentf
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
  "r"   'query-replace-regexp
  "+"   'set-mark-command
  "j"   'ace-jump-line-mode)

;; evil state hooks
(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1 "#ab3737")
             (set-face-background 'mode-line "#6b95b2")
             (aggressive-indent-mode 0)
             (powerline-reset)))

(add-hook 'evil-emacs-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1 "#a0522d")
             (set-face-background 'mode-line "#6b95b2")
             (my/aggressive-indent-if)
             (powerline-reset)))

(add-hook 'evil-visual-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1 "#79596d")
             (set-face-background 'mode-line "#6b95b2")
             (aggressive-indent-mode 0)
             (powerline-reset)))

(add-hook 'evil-insert-state-entry-hook
          '(lambda ()
             (set-face-background 'powerline-active1 "#3d6837")
             (set-face-background 'mode-line "#6b95b2")
             (my/aggressive-indent-if)
             (powerline-reset)))
