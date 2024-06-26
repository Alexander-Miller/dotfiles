;; -*- lexical-binding: t -*-

(defconst S (float-time))
(load (concat (getenv "EMACS_HOME") "modules/bootstrap") nil :no-message)
(std::load "stdlib")
(std::load "text-editing")
(std::load "misc-utils")
(std::load "ui")
(std::load "files-buffers")
(std::load "window-management")
(std::load "completion")
(std::load "selection")
(std::load "error-checking")
(std::load "modeline")
(std::load "help")
(std::load "elisp")
(std::load "projects")
(std::load "vcs")
(std::load "org")
(std::load "org-capture")
(std::load "org-agenda")
(std::load "org-roam")
(std::load "shell")
(std::load "elfeed")
(std::load "dired")
(std::load "finance")
(std::load "lsp")
(std::load "treemacs")
(std::load "docker"     :if (executable-find "docker"))
(std::load "mail"       :if (executable-find "mu"))
(std::load "spellcheck" :if (executable-find "aspell"))
(std::load "rust"       :if (executable-find "cargo"))
(std::load "local"      :if (file-exists-p (expand-file-name "local.el" std::dirs::modules)))
(message "Config loaded in %ss" (- (float-time) S))
