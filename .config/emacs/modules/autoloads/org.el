;; -*- lexical-binding: t -*-

(require 'helm)

(autoload 'helm-org-build-sources "helm-org")

(defun std::org::mode-hook ()
  (org-indent-mode)
  (org-superstar-mode)
  (auto-revert-mode)
  (hl-todo-mode -1)
  (toc-org-mode)
  (rainbow-delimiters-mode-disable))

(defun std::org::inbox-refile-targets (&optional arg)
  (interactive "P")
  (let ((files (list std::org::work-file std::org::private-file)))
    (helm :sources (helm-org-build-sources files nil arg)
          :preselect (helm-org-in-buffer-preselect)
          :truncate-lines helm-org-truncate-lines
          :buffer "*helm org inbuffer*")))

(defun std::org::goto-org-file ()
  (interactive)
  (find-file-existing
   (std::read "Org File: "
     (--map (cons (propertize (f-filename it) :path it) it)
            (std::files std::org-dir ".org"))
     nil :require-match)))

(defun std::org::schedule-now ()
  (interactive)
  (org-schedule nil (current-time)))

(defun std::org::table-recalc ()
  "Reverse the prefix arg bevaviour of `org-table-recalculate', such that
by default the entire table is recalculated, while with a prefix arg recalculates
only the current cell."
  (interactive)
  (setf current-prefix-arg (not current-prefix-arg))
  (call-interactively #'org-table-recalculate))
