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

(defun std::vcs::update-gut-gutter (&rest _)
  (when (and git-gutter-mode
             (not (memq this-command '(git-gutter:stage-hunk
                                       git-gutter:revert-hunk)))
             (not inhibit-redisplay))
    (ignore (git-gutter))))

(evil-define-motion std::vcs::next-5-lines-wrapper ()
  "Move the cursor 5 lines down."
  :type line
  (evil-next-visual-line 5))

(evil-define-motion std::vcs::prev-5-lines-wrapper ()
  "Move the cursor 5 lines down."
  :type line
  (evil-previous-visual-line 5))
