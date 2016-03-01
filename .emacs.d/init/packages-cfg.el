;;; package.el --- package management config

;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes/")

(el-get 'sync)

(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(setq a/el-get-packages
      '(ace-window
	anzu
        avy
	cl-lib
        comment-dwim-2
        company-mode
        company-flx
        company-quickhelp
        company-shell
        dash
        diminish
        dired-hacks
        dired-plus
        dired-rainbow
        dtrt-indent
        elpy
        emacs-fish
        emacs-async
        evil
        evil-anzu
        evil-exchange
        evil-leader
        evil-magit
        evil-matchit
        evil-numbers
        expand-region
        eyebrowse
	f
        flx
        flycheck
        flycheck-pos-tip
        git-gutter
        git-gutter-fringe
        helm
        helm-ag
        helm-c-yasnippet
        helm-flx-git
        helm-projectile
        helm-swoop
        highlight-symbol
        key-chord
        list-utils
        magit
        markdown-mode
        morning-star-theme
        ;; mu4e
        multi-compile
        multi-term
        nlinum
        ;; offlineimap
        org-bullets
        org-mode
        pdf-tools
        pos-tip
        projectile
        rainbow-delimiters
        rainbow-mode
	s
        shackle
        smartparens
        spaceline
        undo-tree
        volume
        vimish-fold
        vimrc-mode
        which-key
        yasnippet))

(el-get 'sync a/el-get-packages)

(provide 'packages-cfg)
;;; packages-cfg.el ends here
