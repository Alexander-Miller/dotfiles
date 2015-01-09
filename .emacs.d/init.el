;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get dependency management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;el-get init
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;;add elpa to el-get
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;;el-get packages
(setq my:el-get-packages
      '(auctex
        reftex
        company
        company-auctex
        company-math
        slime
        slime-company
        powerline
        smooth-scroll
        magit
        god-mode
        helm
        yasnippet
        ))
(el-get 'sync my:el-get-packages)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inferior-lisp-program "clisp")
(setq slime-contribs '(slime-fancy))
(setq slime-contribs '(slime-company))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;activate autocompletion everywhere
(add-hook 'after-init-hook 'global-company-mode)
;;align icons to the right
(setq-default company-tooltip-align-annotations t)

;;hook company-math into latex
(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))
(add-hook 'TeX-mode-hook 'my-latex-mode-setup)

;;company for slime
(slime-setup '(slime-company))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visuals and general preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;theme dir needs to be in load path for tomorrow theme to work
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-eighties t)

(set-default-font "droid sans mono for powerline-11")

;;set tabs to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;show line numbers
(global-linum-mode t)

;;hide tool menu and scroll bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;autocomplete parensp
(electric-pair-mode 1)

;;show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;;cursor settings
(setq-default cursor-type '(bar . 3))
(set-cursor-color "#dd5049")
(blink-cursor-mode 1)

;;powerline theme
(powerline-center-theme)

;;save sessions
(desktop-save-mode 1)

;;smoother scrolling
(setq scroll-step 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;interactive do
(ido-mode 1)

;;set pdf to open via xdg-open
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "xdg-open") (output-html "xdg-open")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;toggle between horizontal and vertical split-window-vertically
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;;toggle different cursor if god-mode is acive
;(defun my-update-cursor ()
;  (setq cursor-type (if (or god-local-mode buffer-read-only)
;                        'box
;                      'bar)))
(defun god-mode-cursor-toggle ()
  ((if (or god-local-mode buffer-read-only)
       ((setq cursor-type 'box))
     ((setq cursor-type 'bar)))))

(add-hook 'god-mode-enabled-hook 'god-mode-cursor-toggle)
(add-hook 'god-mode-disabled-hook 'god-mode-cursor-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x t") 'toggle-window-split)
(global-set-key (kbd "<C-return>") 'company-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-k") 'kill-whole-line)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(global-set-key (kbd "C-ö") 'next-line)
(global-set-key (kbd "C-l") 'backward-char)
(global-set-key (kbd "C-ä") 'forward-char)
(global-set-key (kbd "M-l") 'left-word)
(global-set-key (kbd "M-ä") 'right-word)

(global-set-key (kbd "<escape>") 'god-local-mode)
