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
              (shell-command-to-string "git pull"))
          (std::log (format "Package '%s' not found" it)))))))

(std::log "Building Org Version File")
(let* ((org-build-dir (concat user-emacs-directory "straight/build/org"))
       (build-version-file (concat org-build-dir "/org-version.el")))
  (unless (file-exists-p build-version-file)
    (let ((default-directory (concat user-emacs-directory "straight/repos/org")))
      (shell-command-to-string "make")
      (copy-file "./lisp/org-version.el" build-version-file))))

(std::log "Loading Standard Config")
(load "~/.emacs.d/init.el")

(std::log "Done")
