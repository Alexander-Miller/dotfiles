(load (concat (getenv "EMACS_HOME") "tools/logging.el") nil :no-message)
(std::log "Pruning Build")

(std::log "Loading Standard Config")
(load "~/.emacs.d/init.el")
(std::loud (straight-prune-build))
(std::log "Done")

(save-buffers-kill-emacs)
