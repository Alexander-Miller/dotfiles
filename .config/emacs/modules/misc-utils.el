;; -*- lexical-binding: t -*-

(std::using-packages
 pretty-hydra
 (wttrin :type git :host github :repo "emacle/emacs-wttrin")
 (go-translate :type git :host github :repo "lorniu/go-translate")
 buttercup
 gcmh
 restart-emacs)

(std::autoload misc-utils
  #'std::misc::what-face
  #'std::misc::weather
  #'std::misc::toggles/body
  #'std::misc::goto-xref-and-close-search)

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
  (std::add-advice #'gcmh-idle-garbage-collect :after #'after-focus-change-function))

;; Startup
(setf
 initial-major-mode                'fundamental-mode
 inhibit-startup-message           t
 inhibit-startup-echo-area-message t
 inhibit-default-init              t
 initial-scratch-message           nil)

;; Shutdown
(std::add-hook 'kill-emacs-hook
  (setf kill-ring (-map #'substring-no-properties kill-ring)))

;; UTF8
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setf locale-coding-system    'utf-8
      selection-coding-system 'utf-8)

;; Other
(setf
 calendar-mark-holidays-flag  t
 cl-print-readably            t
 warning-suppress-types       '(comp)
 xref-prompt-for-identifier   nil
 transient-values-file        "~/.config/emacs/transient/transient-values.el"
 make-backup-files            t
 backup-directory-alist       '(("." . "~/.emacs.d/cache"))
 delete-old-versions          t
 kept-new-versions            6
 kept-old-versions            2
 version-control              t
 create-lockfiles             nil
 vc-follow-symlinks           t
 browse-url-browser-function  #'browse-url-firefox
 user-mail-address            "alexanderm@web.de"
 user-full-name               "Alexander Miller")

(put 'narrow-to-region       'disabled            nil)
(put 'conf-assignment-column 'safe-local-variable #'integerp)
(put 'org-list-indent-offset 'safe-local-variable #'integerp)
(put 'fill-column            'safe-local-variable #'integerp)
(put 'toc-org-max-depth      'safe-local-variable #'integerp)

;; BUG: https://lists.endsoftwarepatents.org/archive/html/bug-gnu-emacs/2014-05/msg00597.html
(fset 'epg-wait-for-status 'ignore)

(setq-default
 safe-local-variable-values
 '((eval auto-fill-mode t)))

(defalias #'yes-or-no-p #'y-or-n-p)

(std::schedule 1 :no-repeat
  (require 'server)
  (unless (eq t (server-running-p))
    (server-start)))

(std::keybind
 :global
 "C-x ö" #'std::misc::what-face
 :leader
 "qq"  #'save-buffers-kill-terminal
 "qr"  #'restart-emacs
 "t"   #'std::misc::toggles/body
 "aw"  #'std::misc::weather
 "at"  #'gts-do-translate
 "ac"  #'calendar
 "u"   #'universal-argument
 :keymap evil-normal-state-map
 "M-." #'xref-find-definitions
 "M-," #'xref-go-back
 :evil 'motion xref--xref-buffer-mode-map
 "RET" #'xref-goto-xref
 "<C-return>" #'std::misc::goto-xref-and-close-search)

(std::after wttrin
  (setf wttrin-default-cities '("Stuttgart")
        wttrin-default-accept-language '("en-gb")) )

(std::after go-translate

  (std::add-hook 'gts-after-buffer-prepared-hook
    (evil-motion-state)
    (define-key evil-motion-state-local-map
      "?" (keymap-lookup gts-buffer-local-map "h")))

  (setf
   gts-buffer-follow-p t
   gts-cache-enable    nil
   gts-translate-list '(("en" "de") ("de" "en"))
   gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-bing-engine)
                   (gts-google-engine :parser (gts-google-summary-parser)))
    :render (gts-buffer-render)
    :splitter nil)))
