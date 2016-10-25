;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(vimscript
     html
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     python
     ranger
     rust
     syntax-checking
     shell-scripts
     version-control)
   dotspacemacs-additional-packages
   '(dired+
     shackle
     rainbow-mode
     flycheck-package
     company-quickhelp
     flx
     vimish-fold
     company-flx
     dash
     helm-systemd)
   dotspacemacs-excluded-packages '(evil-org popwin)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https                         t
   dotspacemacs-elpa-timeout                       10
   dotspacemacs-check-for-update                   t
   dotspacemacs-editing-style                      'vim
   dotspacemacs-verbose-loading                    nil
   dotspacemacs-startup-banner                     'random
   dotspacemacs-startup-lists                      '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size           5
   dotspacemacs-scratch-mode                       'emacs-lisp-mode
   dotspacemacs-themes                             '()
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font                       '("Fantasque Sans Mono" :size 18 :weight normal :width normal :powerline-scale 1.3)
   dotspacemacs-leader-key                         "SPC"
   dotspacemacs-emacs-leader-key                   "M-m"
   dotspacemacs-major-mode-leader-key              ","
   dotspacemacs-major-mode-emacs-leader-key        "C-M-m"
   dotspacemacs-distinguish-gui-tab                t
   dotspacemacs-command-key                        ":"
   dotspacemacs-remap-Y-to-y$                      t
   dotspacemacs-default-layout-name                "std"
   dotspacemacs-display-default-layout             nil
   dotspacemacs-auto-resume-layouts                t
   dotspacemacs-auto-save-file-location            'cache
   dotspacemacs-max-rollback-slots                 5
   dotspacemacs-use-ido                            nil
   dotspacemacs-helm-resize                        nil
   dotspacemacs-helm-no-header                     nil
   dotspacemacs-helm-position                      'bottom
   dotspacemacs-enable-paste-micro-state           nil
   dotspacemacs-which-key-delay                    2.0
   dotspacemacs-which-key-position                 'bottom
   dotspacemacs-loading-progress-bar               t
   dotspacemacs-fullscreen-at-startup              nil
   dotspacemacs-fullscreen-use-non-native          nil
   dotspacemacs-maximized-at-startup               nil
   dotspacemacs-active-transparency                100
   dotspacemacs-inactive-transparency              100
   dotspacemacs-mode-line-unicode-symbols          t
   dotspacemacs-smooth-scrolling                   nil
   dotspacemacs-line-numbers                       nil
   dotspacemacs-smartparens-strict-mode            nil
   dotspacemacs-highlight-delimiters               'any
   dotspacemacs-persistent-server                  nil
   dotspacemacs-search-tools                       '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository         nil
   dotspacemacs-whitespace-cleanup                 'all))

(defun dotspacemacs/user-init ()
  (ignore))

(defun dotspacemacs/user-config ()
  (org-babel-load-file (concat (getenv "SPACEMACSDIR") "/user-config.org"))
)
