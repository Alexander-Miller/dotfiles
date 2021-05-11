;; -*- lexical-binding: t -*-

(load (concat (getenv "EMACS_HOME") "tools/logging.el") nil :no-message)
(std::log "Synchronizing Packages")

(let ((args (cdddr command-line-args)))
  (when args
    (dolist (it args)
      (let ((repo-dir (expand-file-name (format "straight/repos/%s" it) user-emacs-directory)))
        (if (file-exists-p repo-dir)
            (let ((default-directory repo-dir))
              (std::log (format "Updating %s" it))
              (unless (= 0 (shell-command "git pull"))
                (std::log (format "Update error: %s"
                                  (with-current-buffer (get-buffer "*Shell Command Output*") (buffer-string))))))
          (std::err (format "Package '%s' not found" it)))))))

(std::log "Building Org Version File")
(let* ((org-build-dir (concat user-emacs-directory "straight/build/org"))
       (build-version-file (concat org-build-dir "/org-version.el"))
       (org-repo-dir (concat user-emacs-directory "straight/repos/org")))
  (unless (or (file-exists-p build-version-file)
              (not (file-exists-p org-repo-dir)))
    (let ((default-directory org-repo-dir))
      (shell-command-to-string "make")
      (copy-file "./lisp/org-version.el" build-version-file))))

(std::log "Recompiling User Config")
(load (concat (getenv "EMACS_HOME") "tools/compile.el") nil :no-message)

(std::log "Creating Bulk Autoloads")
(with-temp-buffer
  (dolist (pkg-dir (directory-files std::pkg-build-dir :full))
    (dolist (file (directory-files pkg-dir :full "autoloads.el"))
      (goto-char (point-max))
      (insert "\n")
      (insert-file-contents file)))
  (f-write (buffer-string) 'utf-8 std::pkg-autoloads-file))

(std::log "Sync Complete")
