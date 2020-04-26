;; -*- lexical-binding: t -*-

(defun std::search::rg-in-project ()
  (interactive)
  (-if-let (dir (projectile-project-root))
      (helm-do-ag dir)
    (message "Not in a project.")))
