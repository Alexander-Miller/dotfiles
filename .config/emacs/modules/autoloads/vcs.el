;; -*- lexical-binding: t -*-

(defun std::vcs::org-reveal-on-visit ()
  (when (eq 'org-mode major-mode)
    (org-reveal)))

(defun std::vcs::magit-pkg-status ()
  (interactive)
  (let* ((alist (--map (cons (f-filename it) it)
                       (std::files (concat user-emacs-directory "straight/repos"))))
         (repo (completing-read "Repo: " alist))
         (path (cdr (assoc repo alist))))
    (when path (magit-status path))))
