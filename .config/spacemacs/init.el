;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '((auto-completion
      :variables
      auto-completion-return-key-behavior        'complete
      auto-completion-tab-key-behavior           'cycle
      auto-completion-complete-with-key-sequence nil
      auto-completion-private-snippets-directory nil
      auto-completion-enable-help-tooltip        'manual
      auto-completion-enable-snippets-in-popup   t)
     better-defaults
     emacs-lisp
     git
     html
     markdown
     org
     pdf-tools
     python
     ranger
     rust
     syntax-checking
     shell-scripts
     (version-control
      :variables
      version-control-diff-tool     'git-gutter
      version-control-global-margin t)
     vimscript
     yaml)
   dotspacemacs-additional-packages
   '(company-flx
     dash
     dired+
     el-mock
     flx
     flycheck-package
     haskell-mode
     helm-systemd
     (i3wm-config-mode   :location (recipe :fetcher github
                                           :repo "Alexander-Miller/i3wm-config-mode"))
     (morning-star-theme :location (recipe :fetcher github
                                           :repo "Alexander-Miller/morning-star-theme"))
     rainbow-mode
     shackle
     vimish-fold
     writeroom-mode)
   dotspacemacs-excluded-packages '(evil-org popwin vi-tilde-fringe)
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
   dotspacemacs-themes                             '(morning-star)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font                       '("Monaco" :size 14 :weight normal :width normal :powerline-scale 1.0)
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
   dotspacemacs-whitespace-cleanup                 'all))

(defun dotspacemacs/user-init ()
  (setq custom-file (concat (getenv "SPACEMACSDIR") "/custom-file.el")))

(defun dotspacemacs/user-config ()
  (load-file (concat (getenv "SPACEMACSDIR") "/user-config.elc")))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)
