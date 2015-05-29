;;; misc-settings-cfg.el --- various minor settings

;;; Commentary:
;;; Code:

(require 'git-gutter-fringe)
(global-auto-revert-mode      t)
(global-git-gutter-mode       t)
(global-hl-line-mode          t)
(global-prettify-symbols-mode t)
(global-visual-line-mode      t)
(yas-global-mode              t)
(blink-cursor-mode            t)
(column-number-mode           t)
(desktop-save-mode            t)
(semantic-mode                t)
(electric-pair-mode           t)
(golden-ratio-mode            t)
(key-chord-mode               t)
(menu-bar-mode                t)
(show-paren-mode              t)
(subword-mode                 t)
(transient-mark-mode          t)
(undo-tree-mode               t)
(horizontal-scroll-bar-mode   0)
(scroll-bar-mode              0)
(tool-bar-mode                0)

(setq-default
 ace-jump-mode-case-fold         nil
 ace-jump-mode-move-keys         '(?a ?s ?d ?f ?q ?w ?e ?x ?c ?h ?j ?k ?l ?n ?m ?i)
 ace-jump-mode-scope             'window
 backup-directory-alist          '((".*" . "~/.emacs.d/backups"))
 blink-cursor-blinks             0
 fill-column                     80
 frame-title-format              '(buffer-file-name "%f" ("%b"))
 fringe-mode                     '(1 . 0)
 indent-tabs-mode                nil
 inhibit-splash-screen           t
 next-line-add-newlines          t
 nlinum-format                   " %d"
 require-final-newline           t
 scroll-conservatively           1000
 scroll-margin                   15
 scroll-preserve-screen-position nil
 show-paren-delay                0
 tab-width                       4
 whitespace-line-column          120
 x-select-enable-clipboard       t)

(load-theme 'morning-star t)

(load "server")
(unless (server-running-p) (server-start))

(set-frame-font "Fantasque Sans Mono 12")
(set-fontset-font "fontset-default" nil
                  (font-spec :size 16 :name "Symbola"))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook '(lambda () (rainbow-delimiters-mode t)))

(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono 12"))

(add-to-list 'auto-mode-alist '("\\.fish\\'" . sh-mode))

(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'switch-window)

(add-to-list 'golden-ratio-exclude-modes "neotree-mode")
(add-to-list 'golden-ratio-exclude-modes "term-mode")
(add-to-list 'golden-ratio-exclude-modes "reftex-toc-mode")
(add-to-list 'golden-ratio-exclude-modes "flycheck-error-list-mode")
(add-to-list 'golden-ratio-inhibit-functions
             '(lambda () (if (boundp 'helm-alive-p) (symbol-value 'helm-alive-p))))

(provide 'misc-settings-cfg)
;;; misc-settings-cfg.el ends here
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "deutsch8")
