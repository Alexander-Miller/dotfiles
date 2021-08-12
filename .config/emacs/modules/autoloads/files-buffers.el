;; -*- lexical-binding: t -*-

(defun std::scratch ()
  (interactive)
  (find-file-existing "~/Dropbox/SCRATCH.el"))

(defun std::can-be-other-buffer? (buf)
  (not (memq (buffer-local-value 'major-mode buf)
             '(mu4e-main-mode
               mu4e-headers-mode
               mu4e-compose-mode
               mu4e-view-mode
               helm-major-mode
               help-mode
               helpful-mode
               org-agenda-mode))))

(defun std::other-buffer ()
  ""
  (interactive)
  (let* ((buf (current-buffer))
         (next (other-buffer buf)))
    (when (not (std::can-be-other-buffer? next))
      (let* ((cap (length (buffer-list)))
             (i 0)
             (found nil))
        (while (and (> cap i)
                    (not found))
          (setf i (1+ i)
                buf next
                next (other-buffer buf))
          (when (std::can-be-other-buffer? next)
            (setf found t)))
        (unless found
          (setf next (find-file-noselect "~/Dropbox/SCRATCH.el")))))
    (switch-to-buffer next)))

(defun std::edit-module ()
  (interactive)
  (let* ((modules (--reject (or (string-suffix-p ".elc" it) (file-directory-p it))
                            (std::files std::module-dir)))
         (modules-alist (--map `(,(file-name-sans-extension (file-name-nondirectory it)) . ,it)
                               modules))
         (autoloads (--reject (string-suffix-p ".elc" it)
                              (std::files std::autoloads-dir)))
         (autoloads-alist (--map `(,(concat (file-name-sans-extension (file-name-nondirectory it)) " autoloads") . ,it)
                                 autoloads))
         (options (append '(("init" . "~/.emacs.d/init.el")
                            ("early init" . "~/.emacs.d/early-init.el"))
                          (nconc modules-alist autoloads-alist)))
         (selection (std::read "Module: "
                      (--map (propertize (car it) :path (cdr it)) options))))
    (-some-> (get-text-property 0 :path selection) (find-file-existing))))

(defun std::move-buffer-to-parent-frame ()
  "Move current child frame's buffer to its parent and close the child frame."
  (interactive)
  (-when-let (parent (frame-parent (selected-frame)))
    (let ((buffer (current-buffer))
          (point (point)))
      (delete-frame (selected-frame))
      (x-focus-frame parent)
      (display-buffer-same-window buffer nil)
      (goto-char point))))

(defun std::dropbox-buffer-cleanup ()
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

(defun std::rename-buffer-file ()
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
               (when (projectile-project-p)
                 (call-interactively #'projectile-invalidate-cache))
               (message (cond ((and file-moved? file-renamed?)
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
