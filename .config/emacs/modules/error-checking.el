;; -*- lexical-binding: t -*-

(std::using-packages
 flycheck
 flycheck-pos-tip
 package-lint)

(std::autoload error-checking
  #'std::err-check::hydra/body
  #'std::err-check::next-error
  #'std::err-check::previous-error)

(autoload 'flycheck-buffer "flycheck.el")

(std::after flycheck

  (evil-set-initial-state 'flycheck-error-list-mode 'motion)

  (std::keybind
   :keymap flycheck-error-list-mode-map
   "q" #'kill-buffer-and-window)

  (flycheck-pos-tip-mode)
  (flycheck-set-indication-mode 'right-fringe)

  (setf
   flycheck-emacs-lisp-load-path       'inherit
   flycheck-check-syntax-automatically '(mode-enabled save idle-change)
   flycheck-idle-change-delay          10
   flycheck-pos-tip-timeout            999)

  (define-fringe-bitmap 'std::flycheck::fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'std::flycheck::fringe-indicator
    :error-list-face 'flycheck-error-list-error
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'std::flycheck::fringe-indicator
    :error-list-face 'flycheck-error-list-warning
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'std::flycheck::fringe-indicator
    :error-list-face 'flycheck-error-list-info
    :fringe-face 'flycheck-fringe-info))

(std::keybind
 :leader
 "E"  #'std::err-check::hydra/body
 "ee" #'flycheck-buffer
 "el" #'flycheck-list-errors
 "ev" #'flycheck-verify-setup
 "ed" #'flycheck-describe-checker
 "ey" #'flycheck-copy-errors-as-kill
 "eE" #'flycheck-mode
 :evil (normal) 'global
 "C-." #'std::err-check::next-error
 "C-," #'std::err-check::previous-error)
