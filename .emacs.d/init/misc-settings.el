;; ======================================
;; minor visuals and general preferences
;; ======================================

;; base theme, many additions in custom-faces.el
(load-theme 'tomorrow-night-eighties t)

;; default font
(set-default-font "Fantasque Sans Mono:pixelsize=20")
(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono:pixelsize=20"))

;; next-line will always add new lines at the end of a buffer
(setq next-line-add-newlines t)

;; set tabs to 4 spacesss
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; powerline theme
(powerline-center-evil-theme)

;; smoother scrolling
(setq scroll-step 1
      scroll-margin 10000
      scroll-conservatively 1000
      scroll-preserve-screen-position 0)

;; title is file name if available otherwise buffernames
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; files always end with a new line
(setq require-final-newline 't)

;; use clipboard and primary selection
(setq x-select-enable-clipboard t)

;; to open files in current session through the terminal
(load "server")
(unless (server-running-p) (server-start))

;; remove needing to type the full 'yes'
(fset 'yes-or-no-p 'y-or-n-p)

;; remove emacs' startup screen
(setq inhibit-splash-screen t)

;; backup copies go to .emacs dir
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; open fish files in sh-mode
(add-to-list 'auto-mode-alist '("\\.fish\\'" . sh-mode))

;; make ace jump case sensitive
(setq ace-jump-mode-case-fold nil)

;; jump only in current buffer
(setq ace-jump-mode-scope 'window)

;; remove fringe, adapt line numbers
(set-fringe-mode '(0 . 0))
(setq nlinum-format "%d ")

;; more convenient chars for ace-jump-mode
(setq ace-jump-mode-move-keys '(?a ?s ?d ?f ?q ?w ?e ?x ?c ?h ?j ?k ?l ?n ?m ?i))

;; trailing whitespac removal
(add-hook 'before-save-hook 'delete-trailing-whitespace)
