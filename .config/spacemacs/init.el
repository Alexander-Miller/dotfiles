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
     (evil-adjust       :location (recipe :fetcher github :repo "troyp/evil-adjust"))
     (company-quickhelp :location (recipe :fetcher github :repo "expez/company-quickhelp"))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(evil-want-fine-undo t)
 '(hl-sexp-background-color "#efebe9")
 '(org-agenda-files
   (quote
    ("~/Documents/git/dotfiles/.config/spacemacs/user-config.org" "~/Dropbox/notes.org")))
 '(package-selected-packages
   (quote
    (shackle powerline spinner org hydra parent-mode projectile seq pkg-info epl smartparens iedit anzu evil goto-chg undo-tree highlight diminish flx bind-map bind-key packed dash s helm avy helm-core async popup package-build yapfify py-isort live-py-mode company-shell zenburn-theme vimrc-mode fish-mode pyvenv pytest pyenv-mode py-yapf pip-requirements hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic helm-systemd morning-star-theme company-flx wgrep smex ivy-hydra counsel-projectile counsel swiper ivy uuidgen org-projectile org-download mwim link-hint git-link evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump column-enforce-mode cargo nil-theme i3wm-config-mode eyebrowse flycheck-package rainbow-mode toml-mode racer f rust-mode flycheck-rust company-racer deferred dired+ ranger orgit magit-gitflow helm-company git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ flycheck-pos-tip evil-magit magit magit-popup git-commit company-statistics company-quickhelp pos-tip ac-ispell toc-org smeargle org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets mmm-mode markdown-toc markdown-mode htmlize helm-gitignore request helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter gh-md flycheck with-editor diff-hl company auto-yasnippet yasnippet auto-complete ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe use-package spacemacs-theme spaceline smooth-scrolling restart-emacs rainbow-delimiters quelpa popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
