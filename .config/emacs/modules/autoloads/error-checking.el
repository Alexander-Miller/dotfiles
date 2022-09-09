;; -*- lexical-binding: t -*-

(evil-define-motion std::err-check::next-error ()
  "Move to the next flycheck error.
Start searching from the top if point is part the last error."
  :jump t
  :type line
  (-let [it (flycheck-next-error-pos 1)]
    (if (and it
             (not (and (equal (point) (1- (point-min)))
                       (equal (1- (point-min)) it))))
        (goto-char it)
      (--if-let (save-excursion
                  (goto-char (point-min))
                  (flycheck-next-error-pos 1))
          (goto-char it)
        (message "No more Flycheck errors.")))))

(evil-define-motion std::err-check::previous-error ()
  "Move to the previous flycheck error.
  Start searching from the bottom if point is part the first error."
  :jump t
  :type line
  (-let [it (flycheck-next-error-pos -1)]
    (if (and it
             (not (and (equal (point) (point-min))
                       (equal (point-min) it))))
        (goto-char it)
      (--if-let (save-excursion
                  (goto-char (point-max))
                  (flycheck-next-error-pos -1))
          (goto-char it)
        (message "No more Flycheck errors.")))))

(defhydra std::err-check::hydra (:exit t :hint t)
  ("e" #'flycheck-mode                "Toggle Mode")
  ("b" #'flycheck-buffer              "Check Buffer")
  ("l" #'flycheck-list-errors         "List Errors")
  ("v" #'flycheck-verify-setup        "Verify Setup")
  ("d" #'flycheck-describe-checker    "Describe Checker")
  ("y" #'flycheck-copy-errors-as-kill "Copy Errors"))
