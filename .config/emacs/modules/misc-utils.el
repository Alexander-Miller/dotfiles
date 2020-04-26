;; -*- lexical-binding: t -*-

(std::using-packages
 pretty-hydra
 wttrin
 buttercup)

(setf
 fast-but-imprecise-scrolling t ;; trial
 make-backup-files            nil
 create-lockfiles             nil
 load-prefer-newer            t
 vc-follow-symlinks           t
 browse-url-browser-function  #'browse-url-firefox
 user-mail-address            "alexanderm@web.de"
 user-full-name               "Alexander Miller")

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

(std::idle-schedule 5 :repeat #'garbage-collect)

(std::autoload misc-utils
  #'std::what-face
  #'std::notify
  #'std::schedule
  #'std::weather
  #'std::toggles/body)


(std::keybind
 :global
 "C-x ö" #'std::what-face
 :leader
 "qq" #'save-buffers-kill-terminal
 "t"  #'std::toggles/body
 "aw" #'std::weather
 "ac" #'calendar
 :keymap evil-normal-state-map
 "M-." #'xref-find-definitions
 "M-," #'xref-pop-marker-stack)

(std::after wttrin
  (setf wttrin-default-cities '("Stuttgart")
        wttrin-default-accept-language '("en-gb")) )
