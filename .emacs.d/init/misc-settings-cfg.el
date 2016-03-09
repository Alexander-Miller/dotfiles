;;; misc-settings-cfg.el --- various minor settings

;;; Commentary:
;;; Code:

(require 'git-gutter-fringe)

(global-anzu-mode             t)
(global-auto-revert-mode      t)
(global-eldoc-mode            t)
(global-git-gutter-mode       t)
(global-prettify-symbols-mode t)
(global-subword-mode          t)
(vimish-fold-global-mode      t)
(yas-global-mode              t)
(async-bytecomp-package-mode  t)
(column-number-mode           t)
(desktop-save-mode            t)
(dired-async-mode             t)
(dtrt-indent-mode             t)
(eyebrowse-mode               t)
(key-chord-mode               t)
(menu-bar-mode                t)
(mouse-avoidance-mode         t)
(semantic-mode                t)
(subword-mode                 t)
(transient-mark-mode          t)
(undo-tree-mode               t)
(which-key-mode               t)
(horizontal-scroll-bar-mode   0)
(scroll-bar-mode              0)
(tool-bar-mode                0)
(blink-cursor-mode            0)

(setq-default
 indent-tabs-mode    nil
 ispell-dictionary   "en_GB"
 ispell-program-name "hunspell"
 fill-column         80
 truncate-lines      t)

(setq
 backup-directory-alist          '((".*" . "~/.emacs.d/backups"))
 blink-cursor-blinks             0
 dtrt-indent-verbosity           1
 frame-title-format              '(buffer-file-name "%f" ("%b"))
 fringe-mode                     8
 highlight-symbol-idle-delay     4
 indent-tabs-mode                nil
 inhibit-splash-screen           t
 next-line-add-newlines          t
 nlinum-format                   " %d "
 pos-tip-background-color        "#444444"
 require-final-newline           t
 scroll-conservatively           50
 scroll-margin                   15
 scroll-preserve-screen-position t
 show-paren-delay                0.2
 tab-width                       4
 undo-outer-limit                42000000
 whitespace-line-column          120
 select-enable-clipboard         t)

(load-theme 'morning-star t)

(load "server")
(unless (server-running-p) (server-start))

(set-frame-font "Fantasque Sans Mono 12")
(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono 12"))
(set-fontset-font "fontset-default" nil (font-spec :size 13 :name "Symbola"))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook  'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook (lambda () (nlinum-mode t)))
(add-hook 'prog-mode-hook (lambda () (rainbow-mode t)))
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'text-mode-hook (lambda () (nlinum-mode t)))
(add-hook 'conf-mode-hook (lambda () (rainbow-mode t)))
(add-hook 'fish-mode-hook (lambda () (rainbow-mode t)))
(add-hook 'sh-mode-hook   (lambda () (rainbow-mode t)))

(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(setq-default
 prettify-symbols-alist
 '(("lambda" . 955)
   ("<=" . "⇐")
   ("=>" . "⇒")
   ("<-" . "←")
   ("->" . "→")))

(provide 'misc-settings-cfg)
;;; misc-settings-cfg.el ends here
