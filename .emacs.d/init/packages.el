;;; package.el --- package management setup

;;; Commentary:
;;; standard el-get setup to administer packages over multiple installations

;;; Code:

(setq-default package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/")
                ("org"   . "http://orgmode.org/elpa/")
                ("gnu"   . "http://elpa.gnu.org/packages/")))

;; acquire el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

;; add custom recipes
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes/")

(el-get 'sync)

;; add elpa to el-get
(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;; el-get packages
(setq my:el-get-packages
      '(
        ace-jump-mode
        ace-window
        aggressive-indent-mode
        anaconda-mode
        auctex
        company
        company-anaconda
        company-auctex
        company-ghc
        company-math
        company-mode
        escreen
        evil
        evil-leader
        evil-matchit
        evil-surround
        flycheck
        flycheck-pos-tip
        ghc-mod
        git-gutter
        git-gutter-fringe
        golden-ratio
        haskell-mode
        helm
        helm-ag
        helm-company
        helm-swoop
        hi2
        key-chord
        magit
        ;;mu4e
        multi-term
        multiple-cursors
        neotree
        nlinum
        offlineimap
        org-bullets
        popup
        powerline
        projectile
        rainbow-delimiters
        rainbow-mode
        reftex
        smart-mode-line
        tomorrow-theme
        undo-tree
        visual-fill-column
        writeroom-mode
        yasnippet))

(el-get 'sync my:el-get-packages)

(provide 'packages)
;;; packages.el ends here
