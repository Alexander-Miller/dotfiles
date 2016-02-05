;;; package.el --- package management config

;;; Commentary:
;;; Code:

(setq-default package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/")
                ("org"   . "http://orgmode.org/elpa/")
                ("gnu"   . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes/")

(el-get 'sync)

(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(setq my:el-get-packages
      '(ace-window
        async
        ;; auctex
        ;; bongo
        company-mode
        company-flx
        ;; company-auctex
        ;; company-ghc
        ;; company-math
        company-quickhelp
        company-shell
        dash
        diminish
        dired-hacks
        dired-plus
        dtrt-indent
        elpy
        emacs-fish
        evil
        evil-anzu
        evil-exchange
        evil-leader
        evil-magit
        evil-matchit
        evil-numbers
        expand-region
        eyebrowse
        flx
        flycheck
        flycheck-pos-tip
        ;; ghc-mod
        git-gutter+
        git-gutter-fringe+
        golden-ratio
        ;; haskell-mode
        helm
        helm-ag
        helm-flx-git
        helm-projectile
        helm-swoop
        highlight-symbol
        key-chord
        list-utils
        magit
        markdown-mode
        morning-star-theme
        ;;mu4e
        multi-compile
        multi-term
        nlinum
        offlineimap
        org-bullets
        org-mode
        pos-tip
        projectile
        racer
        rainbow-delimiters
        rainbow-mode
        rust-mode
        ;; reftex
        shackle
        smartparens
        smooth-scroll
        spaceline
        toml-mode
        undo-tree
        volume
        vimish-fold
        vimrc-mode
        visual-fill-column
        which-key
        yasnippet
        ))

(el-get 'sync my:el-get-packages)

(provide 'packages-cfg)
;;; packages-cfg.el ends here
