;; -*- lexical-binding: t -*-

(std::using-packages
 dired+)

(std::keybind :leader "ad" #'std::dired)

(std::autoload dired
  #'std::dired
  #'std::dired::quit
  #'std::dired::mode-hook
  #'std::dired::open-externally
  #'std::dired::filesize
  #'std::dired::mark-up)

(std::with-desktop
 :check (eq major-mode 'dired-mode)
 :cmd #'std::dired
 :quit #'std::dired::quit)

(add-hook 'dired-mode-hook #'std::dired::mode-hook)

(defvar std::dired::saved-positions nil)
(defvar std::dired::saved-window-config nil)
(defvar std::dired::cache-file (expand-file-name "cache/std-dired-cache" user-emacs-directory))

;; Settings
(std::after dired

  (require 'dired+)

  (treemacs-icons-dired-mode)

  (defhydra std::dired::goto-hydra (:exit t :hint nil)
    ("h" (lambda () (interactive) (dired "~"))           "$HOME")
    ("d" (lambda () (interactive) (dired "~/Documents")) "Documents")
    ("w" (lambda () (interactive) (dired "~/Downloads")) "Downloads")
    ("v" (lambda () (interactive) (dired "~/Videos"))    "Videos")
    ("o" (lambda () (interactive) (dired "~/Dropbox"))   "Dropbox")
    ("p" (lambda () (interactive) (dired "~/Pictures"))  "Pictures")
    ("m" (lambda () (interactive) (dired "~/Music"))     "Music")
    ("M" (lambda () (interactive) (dired "/run/media"))  "/run/media")
    ("q" nil "cancel"))

  (evil-define-state dired
    "Dired state"
    :cursor '(bar . 0)
    :enable (motion))

  (evil-set-initial-state 'dired-mode 'dired)

  (setf
   dired-listing-switches "-alh --group-directories-first"
   dired-compress-files-alist
   '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
     ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
     ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
     ("\\.zip\\'" . "zip %o -r --filesync %i")))

  (std::advice-add #'revert-buffer :after #'dired-do-compress-to :ignore-args)

  (unless (file-exists-p std::dired::cache-file)
    (make-directory (file-name-directory (directory-file-name std::dired::cache-file)) :parents)
    (f-touch std::dired::cache-file))

  (defun std::dired::quit ()
    (interactive)
    (let ((left) (right))
      (winum-select-window-1)
      (setf left default-directory)
      (winum-select-window-2)
      (setf right default-directory
	    std::dired::saved-positions (list left right))
      (unless (f-exists? std::dired::cache-file)
	(f-touch std::dired::cache-file))
      (f-write (format "%s\n%s" left right) 'utf-8 std::dired::cache-file))
    (set-window-configuration std::dired::saved-window-config)
    (--each (buffer-list)
      (when (eq 'dired-mode (buffer-local-value 'major-mode it))
	(kill-buffer it)))))

;; WDired Settings
(std::after wdired

  (defun std::dired::finish-wdired ()
    (interactive)
    (wdired-finish-edit)
    (evil-dired-state))

  (defun std::dired::abort-wdired ()
    (interactive)
    (wdired-abort-changes)
    (evil-dired-state)))

;; Keybinds
(std::after dired

  ;; (defmacro std::dired::dwim-target-wrap (command name)
  ;;   (let* ((command (cadr command))
  ;;          (command-name (symbol-name command)))
  ;;     `(progn
  ;;        (defun ,name (&optional arg)
  ;;          ,(format "Run %s. Set `dired-dwim-target' to t with a prefix arg." command-name)
  ;;          (interactive "P")
  ;;          (-let [dired-dwim-target arg] (call-interactively #',command)))
  ;;        #',name)))

  (std::keybind
   :keymap evil-dired-state-map
   ;; Open/Navigation
   "gh"  #'std::dired::goto-hydra/body
   "h"   #'diredp-up-directory
   "l"   #'dired-find-file
   "RET" #'dired-find-file
   "ox"  #'std::dired::open-externally
   "J"   #'std::evil::forward-five-lines
   "K"   #'std::evil::backward-five-lines
   "q"   #'std::dired::quit
   ;; File CRUD
   "d" #'dired-flag-file-deletion
   "D" #'dired-do-delete
   "m" #'diredp-move-file
   "c" #'dired-do-copy
   "r" #'dired-do-rename
   "s" #'dired-do-symlink
   "x" #'dired-do-flagged-delete
   "Cd" #'dired-create-directory
   ;;Marking
   "M-j" #'dired-mark
   "M-k" #'std::dired::mark-up
   "u"   #'dired-unmark
   "U"   #'dired-unmark-all-marks
   ;; Archives
   "z" #'dired-do-compress
   "Z" #'dired-do-compress-to
   ;; Other
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
     :keymao wdired-mode-map
     "C-c C-c" #'std::dired::finish-wdired
     "C-c C-k" #'std::dired::abort-wdired)))
