;;; dired-cfg.el --- dired config

;;; Commentary:
;;; Code:

(with-eval-after-load "dired"

  (defun dired-mark-backward ()
    "Mark file and move up."
    (interactive)
    (call-interactively 'dired-mark)
    (call-interactively 'dired-previous-line)
    (call-interactively 'dired-previous-line))

  (defun dired-unmark-backward ()
    "Unmark file and move up."
    (interactive)
    (call-interactively 'dired-unmark)
    (call-interactively 'dired-previous-line)
    (call-interactively 'dired-previous-line))

  (defun dired-xdg-open ()
    "Open file under point with default application."
    (interactive)
    (call-process-shell-command
     (concat "xdg-open " (shell-quote-argument (dired-get-filename)) " &")))

  (evil-define-key 'normal dired-mode-map (kbd "M-m") #'dired-mark-backward)
  (evil-define-key 'normal dired-mode-map (kbd "M-u") #'dired-unmark-backward)
  (evil-define-key 'normal dired-mode-map "l"         #'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "h"         #'diredp-up-directory-reuse-dir-buffer)
  (evil-define-key 'normal dired-mode-map (kbd "RET") #'dired-xdg-open)

  ;; TODO
  ;; (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
  ;; (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
  ;; (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  ;; (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  ;; (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  ;; (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  ;; (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)

  (put 'dired-find-alternate-file 'disabled nil)
  (diredp-toggle-find-file-reuse-dir t)

  (setq-default
   dired-listing-switches             "--group-directories-first -alhv"
   dired-omit-mode                    t
   dired-dwim-target                  t
   dired-recursive-copies             t
   dired-dwim-target                  t
   diredp-wrap-around-flag            t
   diredp-hide-details-initially-flag nil)

  (require 'dired-rainbow)
  (dired-rainbow-define audio      "#66aa66" ("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV"))
  (dired-rainbow-define video      "#c0c674" ("webm" "WEBM" "mkv" "MKV" "mpg" "MPG" "mp4" "MP4" "avi" "AVI" "mov" "MOV" "wmv" "m2v" "m4v" "mpeg" "MPEG" "flv" "FLV"))
  (dired-rainbow-define image      "#c97449" ("jpg" "JPG" "bmp" "BMP" "png" "PNG"))
  (dired-rainbow-define archive    "#a374a8" ("zip" "ZIP" "tar" "TAR" "gz" "GZ" "rar" "RAR" "7z" "7Z"))
  (dired-rainbow-define executable "#cc6666" ("py" "PY" "el" "EL" "hs" "HS" "sh" "SH"))
  (dired-rainbow-define-chmod executable-unix "#cc6666" "-.*x.*"))

(provide 'dired-cfg)
;;; dired-cfg.el ends here
