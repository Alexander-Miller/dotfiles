;; -*- lexical-binding: t -*-

(require 'helm)

(defun std::org::mode-hook ()
  (org-indent-mode)
  (org-bullets-mode)
  (auto-revert-mode)
  (rainbow-delimiters-mode-disable))

(defun std::org::goto-org-file ()
  (interactive)
  (helm :prompt "Org File: "
        :buffer "*helm org files*"
        :sources (helm-build-sync-source "Org Files"
                   :candidates (--map (cons (f-filename it) it) (std::files std::org-dir ".org"))
                   :action #'find-file-existing
                   :filtered-candidate-transformer #'helm-fuzzy-highlight-matches)))

(defun std::org::table-recalc ()
  "Reverse the prefix arg bevaviour of `org-table-recalculate', such that
by default the entire table is recalculated, while with a prefix arg recalculates
only the current cell."
  (interactive)
  (setf current-prefix-arg (not current-prefix-arg))
  (call-interactively #'org-table-recalculate))
