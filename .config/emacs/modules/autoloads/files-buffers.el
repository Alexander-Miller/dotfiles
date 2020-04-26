;; -*- lexical-binding: t -*-

(defun std::scratch ()
  (interactive)
  (find-file-existing "~/Dropbox/SCRATCH.el"))

(defun std::can-be-other-buffer? (buf)
  (not (memq (buffer-local-value 'major-mode buf)
             '(mu4e-main-mode mu4e-headers-mode mu4e-compose-mode mu4e-view-mode))))

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
         (options (cons '("init" . "~/.emacs.d/init.el")
                        (nconc modules-alist autoloads-alist)))
         (selection (completing-read
                     "Module: "
                     (--sort (string< (car it) (car other)) options))))
    (find-file-existing (cdr (assoc selection options)))))

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

(defun std::sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))
