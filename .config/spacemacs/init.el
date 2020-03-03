;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  (setq-default
   *SPACEMACSDIR*                         (getenv "SPACEMACSDIR")
   *ORGDIR*                               "~/Documents/Org"
   dotspacemacs-distribution              'spacemacs-base
   dotspacemacs-enable-lazy-installation  'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path  '()
   dotspacemacs-configuration-layers
   `((auto-completion
      :variables
      auto-completion-return-key-behavior        'complete
      auto-completion-tab-key-behavior           'cycle
      auto-completion-complete-with-key-sequence nil
      auto-completion-private-snippets-directory nil
      auto-completion-enable-help-tooltip        'manual
      auto-completion-enable-snippets-in-popup   t)
     better-defaults
     colors
     (elfeed :variables rmh-elfeed-org-files (list (format "%s/Elfeed.org" *ORGDIR*)))
     emacs-lisp
     finance
     git
     github
     helm
     helpful
     html
     lsp
     markdown
     mu4e
     org
     (python :variables python-backend ',(if (executable-find "pyls") 'lsp 'anaconda))
     restclient
     (rust :variables rust-backend 'lsp)
     syntax-checking
     shell
     shell-scripts
     spacemacs-project
     spacemacs-evil
     spacemacs-completion
     (spell-checking
      :variables
      enable-flyspell-auto-completion nil
      spell-checking-enable-by-default nil)
     (version-control
      :variables
      version-control-diff-tool     'git-gutter
      version-control-global-margin t)
     vimscript
     yaml)
   dotspacemacs-additional-packages
   '(ace-window
     anzu
     buttercup
     company-flx
     (dired+ :location (recipe :fetcher github
                               :repo "emacsmirror/dired-plus"))
     doom-modeline
     el-mock
     eros
     evil-collection
     evil-goggles
     evil-surround
     expand-region
     eyebrowse
     flx
     flycheck-package
     german-holidays
     ivy-posframe
     multi-compile
     ht
     pfuture
     posframe
     rainbow-delimiters
     shackle
     smartparens
     swiper
     vimish-fold
     window-purpose
     winum
     wttrin
     writeroom-mode
     (i3wm-config-mode   :location (recipe :fetcher github :repo "Alexander-Miller/i3wm-config-mode"))
     (morning-star-theme :location (recipe :fetcher github :repo "Alexander-Miller/morning-star-theme"))
     (tridactylrc-mode   :location (recipe :fetcher github :repo "Alexander-Miller/tridactylrc-mode"))
     (framey             :location (recipe :fetcher github :repo "Alexander-Miller/framey")))
   dotspacemacs-delete-orphan-packages nil
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(centered-cursor-mode
     diminish
     evil-anzu
     evil-cleverparens
     evil-ediff
     evil-escape
     evil-iedit-state
     evil-lisp-state
     evil-org
     evil-tutor
     evil-unimpaired
     fancy-battery
     golden-ratio
     lsp-treemacs
     neotree
     open-junk-file
     paradox
     popwin
     spaceline
     spaceline-all-the-icons
     symon
     treemacs
     treemacs-evil
     treemacs-magit
     treemacs-projectile
     vi-tilde-fringe
     vim-powerline)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-enable-emacs-pdumper               nil
   dotspacemacs-emacs-pdumper-executable-file      nil
   dotspacemacs-emacs-dumper-dump-file             "spacemacs.pdmp"
   dotspacemacs-elpa-https                         t
   dotspacemacs-elpa-timeout                       10
   dotspacemacs-gc-cons                            '(100000000 0.25)
   dotspacemacs-use-spacelpa                       nil
   dotspacemacs-verify-spacelpa-archives           nil
   dotspacemacs-check-for-update                   nil
   dotspacemacs-elpa-subdirectory                  'emacs-version
   dotspacemacs-editing-style                      'vim
   dotspacemacs-mode-line-theme                    'vanilla
   dotspacemacs-verbose-loading                    nil
   dotspacemacs-startup-banner                     'random
   dotspacemacs-startup-buffer-responsive          nil
   dotspacemacs-startup-lists                      nil
   dotspacemacs-initial-scratch-message            nil
   dotspacemacs-scratch-mode                       'emacs-lisp-mode
   dotspacemacs-themes                             '(morning-star)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font                       '("Fantasque Sans Mono" :size 16 :weight normal :width normal)
   dotspacemacs-leader-key                         "SPC"
   dotspacemacs-emacs-command-key                  "SPC"
   dotspacemacs-ex-command-key                     ":"
   dotspacemacs-emacs-leader-key                   "M-m"
   dotspacemacs-command-key                        ":"
   dotspacemacs-major-mode-leader-key              ","
   dotspacemacs-major-mode-emacs-leader-key        "C-M-m"
   dotspacemacs-distinguish-gui-tab                t
   dotspacemacs-default-layout-name                "@std"
   dotspacemacs-display-default-layout             t
   dotspacemacs-auto-resume-layouts                nil
   dotspacemacs-auto-generate-layout-names         nil
   dotspacemacs-large-file-size                    1
   dotspacemacs-auto-save-file-location            'cache
   dotspacemacs-max-rollback-slots                 5
   dotspacemacs-enable-paste-transient-state       t
   dotspacemacs-which-key-delay                    2.0
   dotspacemacs-which-key-position                 'right
   dotspacemacs-switch-to-buffer-prefers-purpose   nil
   dotspacemacs-loading-progress-bar               nil
   dotspacemacs-fullscreen-at-startup              nil
   dotspacemacs-fullscreen-use-non-native          nil
   dotspacemacs-maximized-at-startup               nil
   dotspacemacs-active-transparency                100
   dotspacemacs-inactive-transparency              100
   dotspacemacs-show-transient-state-title         t
   dotspacemacs-show-transient-state-color-guide   t
   dotspacemacs-mode-line-unicode-symbols          nil
   dotspacemacs-smooth-scrolling                   nil
   dotspacemacs-line-numbers                       (fboundp 'display-line-numbers-mode)
   dotspacemacs-folding-method                     'evil
   dotspacemacs-smartparens-strict-mode            nil
   dotspacemacs-server-socket-dir                  nil
   dotspacemacs-enable-server                      t
   dotspacemacs-highlight-delimiters               'any
   dotspacemacs-smart-closing-parenthesis          nil
   dotspacemacs-persistent-server                  nil
   dotspacemacs-search-tools                       '("ag" "rg" "grep")
   dotspacemacs-frame-title-format                 "Emacs"
   dotspacemacs-icon-title-format                  nil
   dotspacemacs-whitespace-cleanup                 'trailing
   dotspacemacs-zone-out-when-idle                 nil
   dotspacemacs-pretty-docs                        nil))

(defun dotspacemacs/user-init ()
  (setq custom-file (concat (getenv "SPACEMACSDIR") "/custom-file.el")))

(defun dotspacemacs/user-config ()
  (load-file (concat *SPACEMACSDIR* "/user-config.elc"))
  (spacemacs/switch-to-scratch-buffer))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)

