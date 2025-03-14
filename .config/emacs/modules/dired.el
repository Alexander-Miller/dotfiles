;; -*- lexical-binding: t -*-

(std::using-packages
 dired+
 peep-dired)

(std::keybind :leader "ad" #'std::dired)

(std::autoload dired
  #'std::dired
  #'std::dired::quit
  #'std::dired::quit-forget
  #'std::dired::mode-hook
  #'std::dired::open-externally
  #'std::dired::filesize
  #'std::dired::preview
  #'std::dired::file-crud-hydra/body
  #'std::dired::mark-up)

(add-hook 'dired-mode-hook #'std::dired::mode-hook)

;; Must happen *before* dired is loaded
(setf diredp-omit-files-font-lock-regexp ".^")

(std::after dired

  (require 'dired+)

  (add-hook 'dired-mode-hook #'treemacs-icons-dired-enable-once)

  (evil-define-state dired
    "Dired state"
    :cursor '(bar . 0)
    :enable (motion))

  (evil-set-initial-state 'dired-mode 'dired)

  (setf
   dired-listing-switches             "-alh -v --group-directories-first"
   dired-compress-file-default-suffix ".zip"
   dired-compress-file-alist
   '(("\\.zip\\'" . "zip -r %o %i")
     ("\\.gz\\'" . "gzip -9f %i")
     ("\\.bz2\\'" . "bzip2 -9f %i")
     ("\\.xz\\'" . "xz -9f %i")
     ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i")))

  (std::add-advice #'revert-buffer :after #'dired-do-compress-to :ignore-args)
  (std::add-advice #'revert-buffer :after #'dired-do-compress    :ignore-args)

  (defun std::dired::quit ()
    (interactive)
    (--each (buffer-list)
      (when (eq 'dired-mode (buffer-local-value 'major-mode it))
	(kill-buffer it)))))

(std::after wdired

  (defun std::dired::finish-wdired ()
    (interactive)
    (wdired-finish-edit)
    (evil-dired-state))

  (defun std::dired::abort-wdired ()
    (interactive)
    (wdired-abort-changes)
    (evil-dired-state)))

(std::after dired

  (std::keybind
   :keymap evil-dired-state-map
   ;; Open/Navigation
   "gh"  #'std::dired::goto-hydra/body
   "h"   #'diredp-up-directory
   "l"   #'dired-find-file
   "RET" #'dired-find-file
   "ox"  #'std::dired::open-externally
   "J"   #'std::edit::evil-forward-five-lines
   "K"   #'std::edit::evil-backward-five-lines
   "q"   #'std::dired::quit
   "Q"   #'std::dired::quit-forget
   ;; File CRUD
   "d" #'dired-flag-file-deletion
   "x" #'dired-do-flagged-delete
   "f" #'std::dired::file-crud-hydra/body
   ;;Marking
   "M-j" #'dired-mark
   "M-k" #'std::dired::mark-up
   "u"   #'dired-unmark
   "U"   #'dired-unmark-all-marks
   ;; Archives
   "z" #'dired-do-compress
   "Z" #'dired-do-compress-to
   ;; Other
   "p"   #'std::dired::preview
   "gr"  #'revert-buffer
   "I"   #'std::dired::filesize
   "("   #'dired-hide-details-mode
   "["   #'global-dired-hide-details-mode
   :mode-leader dired-mode
   "C-e" #'wdired-change-to-wdired-mode
   :keymap evil-dired-state-map
   ","   std::dired-mode-leader-map)

  (std::after wdired
    (std::keybind
     :keymap wdired-mode-map
     "C-c C-c" #'std::dired::finish-wdired
     "C-c C-k" #'std::dired::abort-wdired)))
