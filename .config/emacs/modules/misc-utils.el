;; -*- lexical-binding: t -*-

(std::using-packages
 pretty-hydra
 (wttrin :type git :host github :repo "emacle/emacs-wttrin")
 buttercup
 gcmh
 restart-emacs)

;; GC
(setf gc-cons-percentage 0.6
      gc-cons-threshold  most-positive-fixnum)
(std::add-transient-hook 'pre-command-hook (gcmh-mode))
(std::after gcmh
  (setf
   gcmh-idle-delay          5
   gcmh-high-cons-threshold (* 128 1024 1024)
   gcmh-verbose             nil
   gc-cons-percentage       0.25)
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))

;; Startup
(setf
 initial-major-mode                'fundamental-mode
 inhibit-startup-message           t
 inhibit-startup-echo-area-message t
 inhibit-default-init              t
 initial-scratch-message           nil)

;; UTF8
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setf locale-coding-system    'utf-8
      selection-coding-system 'utf-8)

;; Other
(setf
 xref-prompt-for-identifier   nil
 transient-values-file        "~/.config/emacs/transient/transient-values.el"
 make-backup-files            t
 backup-directory-alist       '(("." . "~/.emacs.d/backups"))
 delete-old-versions          t
 kept-new-versions            6
 kept-old-versions            2
 version-control              t
 create-lockfiles             nil
 load-prefer-newer            t
 vc-follow-symlinks           t
 browse-url-browser-function  #'browse-url-firefox
 user-mail-address            "alexanderm@web.de"
 user-full-name               "Alexander Miller")

(put 'narrow-to-region 'disabled nil)

(setq-default
 safe-local-variable-values
 '((org-list-indent-offset . 1)
   (fill-column . 120)
   (eval auto-fill-mode t)))

(defalias #'yes-or-no-p #'y-or-n-p)

(std::schedule 1 :no-repeat
  (require 'server)
  (unless (eq t (server-running-p))
    (server-start)))

(std::autoload misc-utils
  #'std::what-face
  #'std::notify
  #'std::schedule
  #'std::weather
  #'std::toggles/body)

(std::keybind
 :global
 "C-x ö" #'std::what-face
 "C-x ü" #'std::ui::change-font
 :leader
 "qq"  #'save-buffers-kill-terminal
 "qr"  #'restart-emacs
 "t"   #'std::toggles/body
 "aw"  #'std::weather
 "ac"  #'calendar
 "u"   #'universal-argument
 "nd"  #'narrow-to-defun
 "nr"  #'narrow-to-region
 "nn"  #'widen
 :keymap evil-normal-state-map
 "M-." #'xref-find-definitions
 "M-," #'xref-pop-marker-stack
 :evil 'motion xref--xref-buffer-mode-map
 "RET" #'xref-goto-xref)

(std::after wttrin
  (setf wttrin-default-cities '("Stuttgart")
        wttrin-default-accept-language '("en-gb")) )

(std::after transient
  (std::keybind
   :keymap transient-base-map
   "<escape>" #'transient-quit-one))
