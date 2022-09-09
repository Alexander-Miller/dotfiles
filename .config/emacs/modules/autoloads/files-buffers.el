;; -*- lexical-binding: t -*-

(defun std::buffers::scratch ()
  "Open the shared scratch buffer."
  (interactive)
  (pop-to-buffer (find-file-noselect "~/Dropbox/SCRATCH.el")))

(defun std::buffers::can-be-other-buffer? (buf)
  "Determine buffers not suited for `std::buffers::other-buffer'."
  (not (memq (buffer-local-value 'major-mode buf)
             '(mu4e-main-mode
               mu4e-headers-mode
               mu4e-compose-mode
               mu4e-view-mode
               dired-mode
               helm-major-mode
               help-mode
               helpful-mode
               messages-buffer-mode
               debugger-mode
               org-roam-mode
               org-agenda-mode))))

(defun std::buffers::other-buffer ()
  "Switch back to last used \"other\" buffer."
  (interactive)
  (let* ((buf (current-buffer))
         (next (other-buffer buf)))
    (when (not (std::buffers::can-be-other-buffer? next))
      (let* ((cap (length (buffer-list)))
             (i 0)
             (found nil))
        (while (and (> cap i)
                    (not found))
          (setf i (1+ i)
                buf next
                next (other-buffer buf))
          (when (std::buffers::can-be-other-buffer? next)
            (setf found t)))
        (unless found
          (setf next (find-file-noselect "~/Dropbox/SCRATCH.el")))))
    (switch-to-buffer next)))

(defun std::buffers::edit-module ()
  "Open one of the dotfiles modules."
  (interactive)
  (let* ((modules (std::files std::dirs::modules "\\.el$"))
         (autoloads (std::files std::dirs::autoloads "\\.el$"))
         (modules-alist (--map
                         `(,(file-name-sans-extension (file-name-nondirectory it)) . ,it)
                         modules))
         (autoloads-alist (--map
                           `(,(concat (file-name-sans-extension (file-name-nondirectory it)) " autoloads") . ,it)
                           autoloads))
         (options (append '(("init" . "~/.emacs.d/init.el")
                            ("early init" . "~/.emacs.d/early-init.el"))
                          (nconc modules-alist autoloads-alist)))
         (selection (std::read "Module: "
                      (--map (propertize (car it) :path (cdr it)) options))))
    (-some-> selection (assoc options) (cdr) (find-file-noselect) (pop-to-buffer))))

(defun std::buffers::move-buffer-to-parent-frame ()
  "Move current child frame's buffer to its parent and close the child frame."
  (interactive)
  (-when-let (parent (frame-parent (selected-frame)))
    (let ((buffer (current-buffer))
          (point (point)))
      (delete-frame (selected-frame))
      (x-focus-frame parent)
      (when (window-dedicated-p)
        (select-window (next-window)))
      (display-buffer-same-window buffer nil)
      (goto-char point))))

(defun std::buffers::dropbox-buffer-cleanup ()
  "Save and kill all dropbox buffers."
  (interactive)
  (let ((dpx (expand-file-name "~/Dropbox"))
        (org (expand-file-name "~/Documents/Org")))
    (dolist (b (buffer-list))
      (-when-let (bf (buffer-file-name b))
        (when (or (s-starts-with? dpx bf)
                  (s-starts-with? org bf))
          (message "Clean up %s" bf)
          (with-current-buffer b
            (save-buffer))
          (kill-buffer b))))))

(defun std::buffers::rename-buffer-file ()
  "Rename the file of the current buffer."
  (interactive)
  (let* ((old-short-name (buffer-name))
         (old-filename (buffer-file-name)))
    (if (not (and old-filename (file-exists-p old-filename)))
        (message "Buffer is not visiting a file")
      (let* ((old-dir        (file-name-directory old-filename))
             (new-name       (read-file-name "New name: " old-filename))
             (new-dir        (file-name-directory new-name))
             (new-short-name (file-name-nondirectory new-name))
             (file-moved?    (not (string= new-dir old-dir)))
             (file-renamed?  (not (string= new-short-name old-short-name))))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              ((string= new-name old-filename)
               (message "Old name is same as new name."))
              (t
               (-let [old-directory (file-name-directory new-name)]
                 (when (and (not (file-exists-p old-directory))
                            (yes-or-no-p (format "Create directory '%s'?" old-directory)))
                   (make-directory old-directory :parents)))
               (rename-file old-filename new-name +1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept old-filename))
               (message
                (cond
                 ((and file-moved? file-renamed?)
                  (concat (std::face "File Moved & Renamed\n" 'font-lock-keyword-face)
                          "From: " (std::face old-filename 'font-lock-string-face) "\n"
                          "To:   " (std::face new-name 'font-lock-string-face)))
                 (file-moved?
                  (concat (std::face "File Moved\n" 'font-lock-keyword-face)
                          "From: " (std::face old-filename 'font-lock-string-face) "\n"
                          "To:   " (std::face new-name 'font-lock-string-face)))
                 (file-renamed?
                  (concat (std::face "File Renamed\n" 'font-lock-keyword-face)
                          "From: " (std::face old-short-name 'font-lock-string-face) "\n"
                          "To:   " (std::face new-short-name 'font-lock-string-face)))))))))))

(defun std::buffers::pop-to-messages-buffer ()
  "Pop to the messages buffer.
Delete it if it is shown already."
  (interactive)
  (-let [buf (messages-buffer)]
    (--if-let (get-buffer-window buf)
        (delete-window it)
      (with-current-buffer buf
        (goto-char (point-max))
        (pop-to-buffer (current-buffer))))))

(defun std::buffers::pop-to-compile-buffer ()
  "Pop to the first compilation buffer.
Delete it if it is shown already."
  (interactive)
  (-if-let (buf (--first
                 (eq 'compilation-mode (buffer-local-value 'major-mode it))
                 (buffer-list)))
      (--if-let (get-buffer-window buf)
          (delete-window it)
        (with-current-buffer buf
          (goto-char (point-max))
          (pop-to-buffer (current-buffer))))
    (message "No compilation buffers.")))

(defun std::buffers::kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))
