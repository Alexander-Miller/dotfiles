;; -*- lexical-binding: t -*-

(defun std::vcs::org-reveal-on-visit ()
  "Reveal org' overlays when jumping from magit."
  (when (eq 'org-mode major-mode)
    (org-reveal)))

(defun std::vcs::magit-pkg-status ()
  "Magit status for a straight-installed package."
  (interactive)
  (let* ((alist (--map (cons (f-filename it) it)
                       (std::files std::dirs::pkg-repos)))
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

(defun std::vcs::ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun std::vcs::ediff-mode-hook ()
  "Ediff mode key setup."
  (ediff-setup-keymap)
  (setf ediff-highlight-all-diffs nil)
  (std::keybind
   :keymap ediff-mode-map
   "j" #'ediff-next-difference
   "k" #'ediff-previous-difference
   "d" #'ediff-jump-to-difference
   "H" #'ediff-toggle-hilit
   "cab" #'ediff-copy-A-to-B
   "cba" #'ediff-copy-B-to-A
   "cac" #'ediff-copy-A-to-C
   "cbc" #'ediff-copy-B-to-C
   "cBc" #'std::vcs::ediff-copy-both-to-C
   "M-J" #'std::vcs::ediff-scroll-down
   "M-K" #'std::vcs::ediff-scroll-up))

(defun std::vcs::ediff-scroll-down ()
  (interactive)
  (let ((last-command-event ?v))
    (ediff-scroll-vertically 1)))

(defun std::vcs::ediff-scroll-up ()
  (interactive)
  (let ((last-command-event ?V))
    (ediff-scroll-vertically 1)))

(pretty-hydra-define std::vcs::ediff-hydra
  (:color blue :quit-key "q" :title (concat (treemacs-get-icon-value "diff") "Ediff"))
  ("Buffers"
   (("b" ediff-buffers  "Buffers")
    ("B" ediff-buffers3 "Buffers (3-way)"))
   "Files"
   (("f" ediff-files        "Files")
    ("F" ediff-files3       "Files (3-way)")
    ("c" ediff-current-file "Current file"))
   "VC"
   (("r" ediff-revision "Revision"))
   "Regions"
   (("l" ediff-regions-linewise "Linewise")
    ("w" ediff-regions-wordwise "Wordwise"))))

(defvar std::vcs::pre-ediff-window-config nil)

(defun std::vcs::save-pre-ediff-window-config ()
  (setf std::vcs::pre-ediff-window-config (current-window-configuration)))

(defun std::vcs::restore-pre-ediff-window-config ()
  (set-window-configuration std::vcs::pre-ediff-window-config))

(defun std::vcs::maybe-setup-commit-buffer ()
  (when (string= (buffer-name) "COMMIT_EDITMSG")
    (require 'git-commit)
    (git-commit-setup-check-buffer)))

(defun std::vcs::copy-current-branch ()
  (interactive)
  (-if-let (branch (magit-get-current-branch))
      (progn (kill-new branch)
             (message "%s" branch))
    (message "There is no current branch")))

(defun std::vcs::copy-current-tag ()
  (interactive)
  (-if-let (tag (magit-get-current-tag))
      (progn (kill-new tag)
             (message "%s" tag))
    (message "There is no current tag")))

(defhydra std::vcs::magit-hydra (:exit t :hint t)
  ("gm" #'magit-dispatch             "Dispatch")
  ("gc" #'magit-clone                "Clone")
  ("gs" #'magit-status               "Status")
  ("gp" #'std::vcs::magit-pkg-status "Pkg Status")
  ("gb" #'magit-blame                "Blame")
  ("gg" #'magit-file-dispatch        "File Dispatch")
  ("gfl" #'magit-log-buffer-file     "Log File")
  ("gff" #'magit-find-file           "Find File"))
