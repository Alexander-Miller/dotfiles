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

(defun std::vcs::ediff-mode-hook ()
  (ediff-setup-keymap)
  (std::keybind
   :keymap ediff-mode-map
   "j" #'ediff-next-difference
   "k" #'ediff-previous-difference
   "d" #'ediff-jump-to-difference
   "H" #'ediff-toggle-hilit
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
