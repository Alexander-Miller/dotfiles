(load (concat (getenv "EMACS_HOME") "tools/logging.el") nil :no-message)
(std::log "Pruning Build")

(std::log "Loading Standard Config")
(load "~/.emacs.d/init.el")

(let ((args (cdddr command-line-args)))
  (when args
    (dolist (it args)
      (let ((build-path (concat std::pkg-build-dir "/" it))
            (repo-path  (concat std::pkg-repos-dir "/" it)))
        (if (file-executable-p repo-path)
            (progn
              (std::log (format "Remove %s" it))
              (f-delete repo-path :recursive)
              (f-delete build-path :recursive))
          (std::err (format "Package %s not found." it)))))))

(std::log "Prune Package Build")
(std::loud (straight-prune-build))

(std::log "Done")

(save-buffers-kill-emacs)
