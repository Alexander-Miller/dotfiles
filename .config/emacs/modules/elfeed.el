;; -*- lexical-binding: t -*-

(std::using-packages
 elfeed
 elfeed-org)

(std::autoload elfeed
  #'std::elfeed::draw-entry
  #'std::elfeed::ignore-entry
  #'std::elfeed::visit-entry-dwim)

(std::with-desktop
 :check (eq major-mode 'elfeed-search-mode)
 :cmd #'elfeed
 :quit #'elfeed-search-quit-window)

(std::keybind :leader "af" #'elfeed)

;; Settings
(std::after elfeed

  (setf rmh-elfeed-org-files (list "~/Documents/Org/Elfeed.org"))

  (elfeed-org)

  (evil-define-state elfeed
    "Evil elfeed state."
    :cursor '(bar . 0)
    :enable (motion))

  (evil-set-initial-state 'elfeed-search-mode 'elfeed)
  (evil-set-initial-state 'elfeed-show-mode 'motion)

  (std::add-hook 'elfeed-search-update-hook (hl-line-highlight))

  (setf
   elfeed-db-directory                (format "%s/Elfeed-DB" std::org-dir)
   elfeed-search-filter               "@6-months-ago -ignore"
   elfeed-search-print-entry-function #'std::elfeed::draw-entry
   elfeed-search-face-alist
   '((unread   elfeed-search-unread-title-face)
     (vids     font-lock-constant-face)
     (blog     font-lock-doc-face)
     (reddit   font-lock-variable-name-face)
     (webcomic font-lock-builtin-face)))

  (doom-modeline-def-segment std::elfeed::dummy-header "" " ")
  (doom-modeline-def-modeline 'empty '(std::elfeed::dummy-header))
  (setf elfeed-search-header-function #'doom-modeline-format--empty))

;; Keybinds
(std::after elfeed
  (std::keybind
    :keymap evil-elfeed-state-map
    "gr"  #'elfeed-update
    "J"   #'std::evil::forward-five-lines
    "K"   #'std::evil::backward-five-lines
    "y"   #'elfeed-search-yank
    "i"   #'std::elfeed::ignore-entry
    "b"   #'std::elfeed::visit-entry-dwim
    "RET" #'elfeed-search-show-entry
    :mode-leader elfeed-search-mode
    "C-o" #'std::elfeed::visit-entry-dwim))
