;; -*- lexical-binding: t -*-

(defun std::search::rg-in-project ()
  "Ripgrep search in the current project."
  (interactive)
  (-if-let (dir (-some-> (project-current) (project-root)))
      (helm-do-ag dir)
    (message "Not in a project.")))
