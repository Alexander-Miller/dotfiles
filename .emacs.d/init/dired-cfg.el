;;; dired-cfg.el --- dired config

;;; Commentary:
;;; Code:

(with-eval-after-load "dired"

  (defun a/dedicated-dired ()
    "Switch to or create dedicated dired buffer. Open dired in current buffer's location if prefix arg is provided."
    (interactive)
    (if current-prefix-arg (dired-jump)
      (let ((dired-buffer
             (cl-find-if
              (lambda (e) (string= "dired-mode" (buffer-local-value 'major-mode (get-buffer e))))
              (helm-buffer-list))))
        (if dired-buffer
            (switch-to-buffer dired-buffer)
          (dired "")))))

  (defun a/dired-mark-backward ()
    "Mark file and move up."
    (interactive)
    (call-interactively 'dired-mark)
    (call-interactively 'dired-previous-line)
    (call-interactively 'dired-previous-line))

  (defun a/dired-unmark-backward ()
    "Unmark file and move up."
    (interactive)
    (call-interactively 'dired-unmark)
    (call-interactively 'dired-previous-line)
    (call-interactively 'dired-previous-line))

  (defun a/dired-xdg-open ()
    "Open file under point with default application."
    (interactive)
    (call-process-shell-command
     (concat "xdg-open " (shell-quote-argument (dired-get-filename)) " &")))

  (defun a/dired-hook ()
    (hl-line-mode t))

  (add-hook 'dired-mode-hook #'a/dired-hook)

  (evil-define-key 'normal dired-mode-map (kbd "RET") #'a/dired-xdg-open)
  (evil-define-key 'normal dired-mode-map (kbd "M-m") #'a/dired-mark-backward)
  (evil-define-key 'normal dired-mode-map (kbd "M-u") #'a/dired-unmark-backward)
  (evil-define-key 'normal dired-mode-map "i"         #'dired-subtree-cycle)
  (evil-define-key 'normal dired-mode-map "l"         #'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "h"         #'diredp-up-directory-reuse-dir-buffer)
  (evil-define-key 'normal dired-mode-map "J"         #'a/quick-forward)
  (evil-define-key 'normal dired-mode-map "K"         #'a/quick-backward)
  (evil-define-key 'normal dired-mode-map "n"         #'evil-search-next)
  (evil-define-key 'normal dired-mode-map "N"         #'evil-search-previous)
  (evil-define-key 'normal dired-mode-map "Q"         #'kill-this-buffer)

  (global-set-key            [f2] #'a/dedicated-dired)
  (define-key dired-mode-map [f2] #'evil-buffer)
  (define-key dired-mode-map [f1] #'shell-toggle)

  (put 'dired-find-alternate-file 'disabled nil)
  (diredp-toggle-find-file-reuse-dir t)

  (setq
   dired-listing-switches             "--group-directories-first -alhv"
   dired-omit-mode                    t
   dired-dwim-target                  t
   dired-recursive-copies             t
   diredp-wrap-around-flag            t
   diredp-hide-details-initially-flag nil)

  (require 'dired-rainbow)
  (dired-rainbow-define audio      "#66aa66" ("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV"))
  (dired-rainbow-define video      "#f7ca88" ("webm" "WEBM" "mkv" "MKV" "mpg" "MPG" "mp4" "MP4" "avi" "AVI" "mov" "MOV" "wmv" "m2v" "m4v" "mpeg" "MPEG" "flv" "FLV"))
  (dired-rainbow-define image      "#c97449" ("jpg" "JPG" "bmp" "BMP" "png" "PNG"))
  (dired-rainbow-define archive    "#a374a8" ("zip" "ZIP" "tar" "TAR" "gz" "GZ" "rar" "RAR" "7z" "7Z"))
  (dired-rainbow-define executable "#cc6666" ("py" "PY" "el" "EL" "hs" "HS" "sh" "SH" "js" "JS" "fish"))
  (dired-rainbow-define-chmod executable-unix "#cc6666" "-.*x.*"))

(with-eval-after-load 'tar-mode

  (evil-set-initial-state 'tar-mode 'motion)
  (evil-add-hjkl-bindings tar-mode-map 'motion
    "q" #'quit-window
    "k" #'tar-previous-line
    "J" #'my/quick-forward
    "K" #'my/quick-backward))

(with-eval-after-load 'archive-mode

  (evil-set-initial-state 'archive-mode 'motion)
  (evil-add-hjkl-bindings archive-mode-map 'motion
    "q" #'quit-window
    "k" #'archive-previous-line
    "J" #'my/quick-forward
    "K" #'my/quick-backward))

(provide 'dired-cfg)
;;; dired-cfg.el ends here
