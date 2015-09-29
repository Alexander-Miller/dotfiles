;;; misc-settings-cfg.el --- various minor settings

;;; Commentary:
;;; Code:

(require 'git-gutter-fringe)

(global-anzu-mode             t)
(global-auto-revert-mode      t)
(global-eldoc-mode            t)
(global-git-gutter-mode       t)
(global-hl-line-mode          t)
(global-prettify-symbols-mode t)
(global-subword-mode          t)
(global-visual-line-mode      t)
(yas-global-mode              t)
(column-number-mode           t)
(company-quickhelp-mode       t)
(desktop-save-mode            t)
(dtrt-indent-mode             t)
(golden-ratio-mode            t)
(key-chord-mode               t)
(menu-bar-mode                t)
(semantic-mode                t)
(shackle-mode                 t)
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
 default-indicate-empty-lines    t
 dtrt-indent-verbosity           0
 fill-column                     80
 frame-title-format              '(buffer-file-name "%f" ("%b"))
 fringe-mode                     '(8 . 0)
 highlight-symbol-idle-delay     4
 indent-tabs-mode                nil
 inhibit-splash-screen           t
 ispell-dictionary               "en_GB"
 ispell-program-name             "hunspell"
 next-line-add-newlines          t
 nlinum-format                   " %d"
 pos-tip-background-color        "#444444"
 require-final-newline           t
 scroll-conservatively           1000
 scroll-margin                   15
 scroll-preserve-screen-position nil
 show-paren-delay                0
 tab-width                       4
 undo-outer-limit                42000000
 whitespace-line-column          120
 x-select-enable-clipboard       t)

(load-theme 'morning-star t)

(load "server")
(unless (server-running-p) (server-start))

(set-frame-font "Fantasque Sans Mono 12")
(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono 12"))
(set-fontset-font "fontset-default" nil
                  (font-spec :size 16 :name "Symbola"))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook  'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook (lambda () (nlinum-mode t)))
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'text-mode-hook (lambda () nlinum-mode t))

(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'switch-window)

(add-to-list 'golden-ratio-exclude-modes "neotree-mode")
(add-to-list 'golden-ratio-exclude-modes "haskell-interactive-mode")
(add-to-list 'golden-ratio-exclude-modes "term-mode")
(add-to-list 'golden-ratio-exclude-modes "reftex-toc-mode")
(add-to-list 'golden-ratio-exclude-modes "flycheck-error-list-mode")
(add-to-list 'golden-ratio-inhibit-functions
             '(lambda () (if (boundp 'helm-alive-p) (symbol-value 'helm-alive-p))))

(defconst hl-line-exclude-mode-hooks
  '(magit-status-mode-hook magit-log-mode-hook org-mode-hook term-mode-hook undo-tree-visualizer-mode-hook)
  "Major modes where hl-line mode will be locally turned off.")

(defun locally-turn-off-hl-line-mode ()
  "Locally turn off 'hl-line-mode'."
  (setq-local global-hl-line-mode nil))

(dolist (hook hl-line-exclude-mode-hooks)
  (remove-hook hook #'locally-turn-off-hl-line-mode)
  (add-hook    hook #'locally-turn-off-hl-line-mode))

(defadvice quit-window (after quit-window-golden-ratio-compatibility activate)
  "Triggers golden ratio after every quit window event."
  (golden-ratio))

(defadvice shackle-display-buffer (after shackle-gg activate)
  "Triggers golden ratio after every buffer display event."
  (golden-ratio))

(setq shackle-rules
      '((".*cider-repl.*"  :regexp t :ratio    0.33)
        (".*cider-error.*" :regexp t :noselect t)
        (".*magit-diff.*"  :regexp t :noselect t)
        ("*Help*"          :select t)))

(setq shackle-default-rule '(:select t))

(setq-default
 prettify-symbols-alist
 '(("lambda" . 955)
   ("<=" . "⇐")
   ("=>" . "⇒")
   ("<-" . "←")
   ("->" . "→")))

(provide 'misc-settings-cfg)
;;; misc-settings-cfg.el ends here
