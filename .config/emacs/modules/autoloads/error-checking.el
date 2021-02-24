;; -*- lexical-binding: t -*-

(evil-define-motion std::flycheck::next-error ()
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

(evil-define-motion std::flycheck::previous-error ()
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
