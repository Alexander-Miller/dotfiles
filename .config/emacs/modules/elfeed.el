;; -*- lexical-binding: t -*-

(std::using-packages
 elfeed
 elfeed-org)

(std::autoload elfeed
  #'std::elfeed
  #'std::elfeed::draw-entry
  #'std::elfeed::ignore-entry
  #'std::elfeed::visit-entry-dwim)

(std::keybind :leader "af" #'std::elfeed)

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
   elfeed-db-directory                (format "%s/Elfeed-DB" std::dirs::org)
   elfeed-search-filter               "@6-months-ago -ignore"
   elfeed-search-print-entry-function #'std::elfeed::draw-entry
   elfeed-search-face-alist
   '((unread   elfeed-search-unread-title-face)
     (vids     font-lock-constant-face)
     (work     font-lock-variable-name-face)
     (blog     font-lock-doc-face)
     (webcomic font-lock-builtin-face)))

  (elfeed-db-ensure))

(std::after elfeed
  (std::keybind
    :keymap evil-elfeed-state-map
    "gr"  #'elfeed-update
    "J"   #'std::edit::evil-forward-five-lines
    "K"   #'std::edit::evil-backward-five-lines
    "y"   #'elfeed-search-yank
    "i"   #'std::elfeed::ignore-entry
    "b"   #'std::elfeed::visit-entry-dwim
    "RET" #'elfeed-search-show-entry
    "+"   #'elfeed-search-tag-all
    "-"   #'elfeed-search-untag-all))
