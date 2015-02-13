;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get dependency management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)

;;el-get init
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
(el-get 'sync)

;;add elpa to el-get
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;;el-get packages
(setq my:el-get-packages
      '(
        ace-jump-mode
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
        ghc-mod
        god-mode
        haskell-mode
        helm
        hi2
        magit
        mu4e
        multiple-cursors
        neotree
        offlineimap
        powerline
        rainbow-mode
        reftex
        tomorrow-theme
        yasnippet
        ))
(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visuals and general preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'tomorrow-night-eighties t)

(set-default-font "droid sans mono for powerline-11")

;;set tabs to 4 spacesss
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;show line numbers
(global-linum-mode t)

;;hide tool menu and scroll bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;autocomplete parens
(electric-pair-mode 1)

;;show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;;cursor settings
(setq-default cursor-type '(bar . 2))
(set-cursor-color "#00ff00")
(blink-cursor-mode 1)
(setq blink-cursor-blinks' 999)

;;powerline theme
(powerline-center-theme)

;;save sessions
(desktop-save-mode 1)

;;smoother scrolling
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;title is file name if available otherwise buffernames
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;files always end with a new line
(setq require-final-newline 't)

(setq x-select-enable-clipboard t)

(aggressive-indent-global-mode t)

;;to open files in current session through the terminal
(server-start)

(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)

(setq-default company-tooltip-align-annotations t)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t :background "#666666" :foreground "#dfdfdf")))
   `(company-tooltip-selection ((t :background "RoyalBlue4" :foreground "#dfdfdf")))
   `(company-tooltip-mouse ((t :background "RoyalBlue4" :foreground "#000000")))
   `(company-tooltip-common ((t :background "#666666" :foreground "#000000")))
   `(company-tooltip-common-selection ((t :background "RoyalBlue4" :foreground "#000000")))
   `(company-tooltip-annotation ((t :background "#666666" :foreground "#33ff33")))
   `(company-scrollbar-fg ((t :background "#c82829")))
   `(company-scrollbar-bg ((t :background "#444444")))
   `(company-preview ((t :background nil :foreround "#f99157")))
   `(company-preview-common ((t :background nil :foreground "#00ffff")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;document parsing to access latex packages
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;AUCTeX multidoc awareness for \input
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

;;activate reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'latex-math-mode)
(setq reftex-plug-into-AUCTeX t)

(company-auctex-init)

;;setup qpdf aus standard viewer
;;(setq TeX-view-program-selection
;;'((output-pdf "PDF Viewer")))
;;(setq TeX-view-program-list
;;'(("PDF Viewer" "qpdfview --unique %o")))

;;proper indentation of list items
(setq LaTeX-item-indent 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook 'anaconda-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-process-type 'cabal-repl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq helm-split-window-in-side-p nil)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-ff-search-library-in-sexp t)
(setq helm-scroll-amount 8)
(setq helm-ff-file-name-history-use-recentf t)

(setq helm-autoresize-max-height '100)
(setq helm-autoresize-min-height '50)

(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cua-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;rebind to make way for autocompleteion at C-return
(add-hook 'cua-mode-hook
          '(lambda ()
             (define-key cua--rectangle-keymap (kbd "C-.") 'cua-clear-rectangle-mark)
             (define-key cua--region-keymap    (kbd "C-.") 'cua-toggle-rectangle-mark)
             (define-key cua-global-keymap     (kbd "C-.") 'cua-set-rectangle-mark)
             (define-key cua--rectangle-keymap [(control return)] nil)
             (define-key cua--region-keymap    [(control return)] nil)
             (define-key cua-global-keymap     [(control return)] nil)))
(cua-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-indent-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; escreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'escreen-goto-screen-hook 'escreen-enable-number-mode-if-more-than-one-screen)
(escreen-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;toggle between horizontal and vertical split-window-vertically

;;switch cursor color in god-mode
(defun god-mode-cursor-toggle ()
  (if (or god-local-mode buffer-read-only)
      (set-cursor-color "#ff0000")
    (set-cursor-color "#00ff00")))
(add-hook 'god-mode-enabled-hook 'god-mode-cursor-toggle)
(add-hook 'god-mode-disabled-hook 'god-mode-cursor-toggle)

(defun my-make-line ()
  (interactive)
  (save-excursion
    (next-line)
    (move-beginning-of-line 1)
    (newline-and-indent)))

(defun my-helm-map-lines(key-map)
  (define-key key-map (kbd "C-p") 'helm-previous-line)
  (define-key key-map (kbd "C-ö") 'helm-next-line))

(defun my-next-line-recenter ()
  (interactive)
  (next-line)
  (recenter))

(defun my-previous-line-recenter ()
  (interactive)
  (previous-line)
  (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x t") 'toggle-window-split)
(global-set-key (kbd "<C-return>") 'company-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-j") 'my-make-line)
(global-set-key (kbd "C-x l") 'recenter-top-bottom)
(global-set-key (kbd "M-ö") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;;helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-S-p") 'helm-semantic-or-imenu)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) 
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-q") 'helm-find-files-grep)
(define-key helm-grep-map (kbd "C-l") 'backward-char)
(define-key helm-grep-map (kbd "C-x l") 'helm-recenter-top-bottom-other-window)

;;TODO: find out how to get these maps to loop
(my-helm-map-lines helm-map)
(my-helm-map-lines helm-find-files-map)
(my-helm-map-lines helm-buffer-map)

;;haskell
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f7] 'haskell-navigate-imports))
(eval-after-load 'haskell-mode
  '(progn 
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;;navigation
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-l") 'backward-char)
(global-set-key (kbd "C-ö") 'next-line)
(global-set-key (kbd "C-ä") 'forward-char)
(global-set-key (kbd "M-l") 'left-word)
(global-set-key (kbd "M-ä") 'right-word)
(global-set-key (kbd "C-ü") 'backward-delete-char-untabify)
(global-set-key (kbd "M-ü") 'backward-kill-word)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "C-M-ä") 'forward-sexp)
(global-set-key (kbd "C-M-l") 'backward-sexp)        

;;ace-jump
(global-set-key (kbd "C-x j") 'ace-jump-line-mode)
(global-set-key (kbd "C-x C-j") 'ace-jump-char-mode)

;;windove buffer switching
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)

(global-set-key "\C-c\C-a" 'mark-whole-buffer)

(global-set-key (kbd "<escape>") 'god-mode-all)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;;escreen
(define-key global-map (kbd "C-z") nil)
(setq escreen-prefix-char (kbd "C-<tab>"))
(define-key escreen-map (kbd "<tab>") 'escreen-goto-next-screen)

;;magit
;;(define-key magit-status-mode-map (kbd "ö") 'magit-goto-next-section)
(define-key magit-mode-map (kbd "ö") 'magit-goto-next-section)

(global-set-key [f8] 'neotree-toggle)

;; open dirs in dired in current window
(put 'dired-find-alternate-file 'disabled nil)

(defface ace-jump-face-foreground
  '((((class color)) (:foreground "#f2777a" :underline nil))
    (((background dark)) (:foreground "gray100" :underline nil))
    (((background light)) (:foreground "gray0" :underline nil))
    (t (:foreground "gray100" :underline nil)))
  "Face for foreground of AceJump motion"
  :group 'ace-jump)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" default)))
 '(fci-rule-color "#393939"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;;remove atrocious green highlight color
 '(highlight ((t :background "#3f3f3f")))
 
 '(company-preview ((t :background nil :foreround "#f99157")))
 '(company-preview-common ((t :background nil :foreground "#00ffff")))
 '(company-scrollbar-bg ((t :background "#444444")))
 '(company-scrollbar-fg ((t :background "#c82829")))
 '(company-tooltip ((t :background "#666666" :foreground "#dfdfdf")))
 '(company-tooltip-annotation ((t :background "#666666" :foreground "#33ff33")))
 '(company-tooltip-common ((t :background "#666666" :foreground "#000000")))
 '(company-tooltip-common-selection ((t :background "RoyalBlue4" :foreground "#000000")))
 '(company-tooltip-mouse ((t :background "RoyalBlue4" :foreground "#000000")))
 '(company-tooltip-selection ((t :background "RoyalBlue4" :foreground "#dfdfdf"))))
