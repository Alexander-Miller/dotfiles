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
        aggressive-indent-mode
        auctex
        bongo
        company
        company-auctex
        company-ghc
        company-math
        company-quickhelp
        define-word
        diminish
        dired-hacks
        dired-plus
        dtrt-indent
        elpy
        escreen
        evil
        evil-anzu
        evil-exchange
        evil-leader
        evil-matchit
        evil-numbers
        evil-smartparens
        evil-surround
        expand-region
        flycheck
        ghc-mod
        git-gutter
        git-gutter-fringe
        golden-ratio
        haskell-mode
        helm
        helm-ag
        helm-company
        helm-swoop
        highlight-symbol
        key-chord
        magit
        markdown-mode
        morning-star-theme
        ;;mu4e
        multi-term
        multiple-cursors
        neotree
        nlinum
        offlineimap
        org-bullets
        org-mode
        powerline
        projectile
        rainbow-delimiters
        rainbow-mode
        reftex
        smart-mode-line
        smartparens
        undo-tree
        volume
        vimrc-mode
        visual-fill-column
        writeroom-mode
        yasnippet
        ))

(el-get 'sync my:el-get-packages)

(provide 'packages-cfg)
;;; packages-cfg.el ends here
