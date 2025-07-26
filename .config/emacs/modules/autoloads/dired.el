;; -*- lexical-binding: t -*-

(autoload 'peep-dired-display-file-other-window "peep-dired")

(std::defun-with-desktop
 :name std::dired
 :command #'std::dired::open
 :check (eq major-mode 'dired-mode))

(defvar std::dired::last-location nil)

(defun std::dired::mode-hook ()
  (setq diredp-hide-details-initially-flag t)
  (hl-line-mode)
  (use-local-map (make-sparse-keymap)))

(defun std::dired::open (&optional arg)
  (interactive "P")
  (-let [use-file-dialog nil]
    (dired (if arg
               (read-directory-name "Dir: ")
             std::dired::last-location))))

(defun std::dired::quit ()
  "Quit and kill all dired-mode buffers."
  (interactive)
  (setf std::dired::last-location default-directory)
  (--each (buffer-list)
    (when (eq 'dired-mode (buffer-local-value 'major-mode it))
      (kill-buffer it)))
  (eyebrowse-switch-to-window-config 1))

(defun std::dired::quit-forget ()
  "Quit and kill all dired-mode buffers and forget dired location."
  (interactive)
  (setf std::dired::last-location nil)
  (--each (buffer-list)
    (when (eq 'dired-mode (buffer-local-value 'major-mode it))
      (kill-buffer it))))

(autoload #'mailcap-extension-to-mime "mailcap")
(defun std::dired::open-externally ()
  (interactive)
  (let* ((files (or (dired-get-marked-files :local)
                    (dired-get-filename)))
         (types (->> files
                     (-map #'file-name-extension)
                     (-map #'mailcap-extension-to-mime)))
         (videos? (--all? (s-starts-with? "video" it) types))
         (cmd (if videos? "mpv" "xdg-open")))
    (call-process-shell-command
     (format "%s %s &"
             cmd
             (->> files
                  (-map #'shell-quote-argument)
                  (s-join " "))))))

(defun std::dired::filesize ()
  "Show filesize using either `du' or `ls'."
  (interactive)
  (-if-let (file (dired-get-filename nil :no-error))
      (let* ((cmd (if (f-directory? file) "du -sh \"%s\"" "ls -sh \"%s\""))
             (output (->> file
                          (format cmd)
                          (shell-command-to-string)
                          (s-trim))))
        (-let [(size file) (s-split-up-to (rx (1+ whitespace)) output 1)]
          (message
           "%s : %s"
           (std::face file 'font-lock-keyword-face)
           (std::face size 'font-lock-string-face))))
    (message (std::face "---" 'font-lock-string-face))))

(defun std::dired::mark-up ()
  (interactive)
  (call-interactively #'dired-mark)
  (forward-line -2))

(defhydra std::dired::goto-hydra (:exit t :hint nil)
  ("h" (dired "~")           "$HOME")
  ("d" (dired "~/Documents") "Documents")
  ("w" (dired "~/Downloads") "Downloads")
  ("v" (dired "~/Videos")    "Videos")
  ("o" (dired "~/Dropbox")   "Dropbox")
  ("p" (dired "~/Pictures")  "Pictures")
  ("m" (dired "~/Music")     "Music")
  ("M" (dired "/run/media")  "/run/media")
  ("q" nil "cancel"))

(defhydra std::dired::file-crud-hydra (:exit t :hint nil)
  ("d"  #'dired-do-delete         "Delete")
  ("m"  #'dired-do-rename         "Move/Rename")
  ("y"  #'dired-do-copy           "Copy")
  ("s"  #'dired-do-symlink        "Symlink")
  ("cd" #'dired-create-directory  "Create Directory")
  ("cf" #'dired-create-empty-file "Create File"))

(defun std::dired::preview ()
  (interactive)
  (peep-dired-display-file-other-window))

(defun std::dired::stop-preview-on-buffer-kill ()
  (when std::dired::preview-timer
    (cancel-timer std::dired::preview-timer)
    (setf std::dired::preview-timer nil)))
