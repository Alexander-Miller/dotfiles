;; -*- lexical-binding: t -*-

(autoload 'peep-dired-display-file-other-window "peep-dired")

(defun std::dired::mode-hook ()
  (setq diredp-hide-details-initially-flag t)
  (hl-line-mode)
  (use-local-map (make-sparse-keymap))
  (std::dired::setup-preview))

(defun std::dired (&optional dir)
  (interactive)
  (setf std::dired::saved-window-config (current-window-configuration))
  (delete-other-windows)
  (unless std::dired::saved-positions
    (-let [(left right)
	   (->> std::dired::cache-file
		(f-read)
		(s-split "\n"))]
      (setf std::dired::saved-positions (list (or left "") (or right "")))))
  (-let [(left right) (--map (if (file-exists-p it) it "~")
			     std::dired::saved-positions)]
    (dired (or dir left))
    (save-selected-window
      (select-window (split-window-right))
      (dired right))))

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
  (interactive)
  (-if-let (file (dired-get-filename nil :no-error))
      (let* ((cmd (if (f-directory? file) "du -sh \"%s\"" "ls -sh \"%s\""))
             (output (->> file
                          (format cmd)
                          ;; (shell-quote-argument)
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

(defvar std::dired::preview-timer nil)

(define-minor-mode std::dired::preview-mode ""
  :init-value t
  :global     t
  :group      'std)

(defun std::dired::setup-preview ()
  (add-hook 'post-command-hook #'std::dired::preview-on-post-command nil :local)
  (add-hook 'kill-buffer-hook #'std::dired::stop-preview-on-buffer-kill nil :local))

(defun std::dired::preview-on-post-command ()
  (when std::dired::preview-mode
    (if std::dired::preview-timer
        (timer-set-time std::dired::preview-timer
                        (time-add (current-time) (seconds-to-time 0.5)))
      (setf std::dired::preview-timer
            (std::schedule 0.5 :no-repeat
              (condition-case _
                  (when (= 1 (winum-get-number))
                    (while-no-input
                      (peep-dired-display-file-other-window)))
                (error (ignore)))
              (setf std::dired::preview-timer nil))))))

(defun std::dired::stop-preview-on-buffer-kill ()
  (when std::dired::preview-timer
    (cancel-timer std::dired::preview-timer)
    (setf std::dired::preview-timer nil)))
