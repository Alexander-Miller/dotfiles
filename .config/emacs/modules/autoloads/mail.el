;; -*- lexical-binding: t -*-

(defun std::mail::compose-mode-hook ()
  (use-hard-newlines -1))

(defun std::mail::view-mode-hook ()
  (visual-line-mode))

(defun std::mail::add-tag (&optional arg)
  (interactive "P")
  (-if-let (msg (mu4e-message-at-point :no-error))
      (-let [tags (if arg
                      "+todo"
                    (->> (read-string "Tag: ")
                         (s-split (rx (1+ " ")))
                         (--map (format "+%s" it))
                         (s-join ",")))]
        (mu4e-action-retag-message msg tags))
    (message "No message here")))

(defun std::mail::remove-tag ()
  (interactive)
  (-if-let (msg (mu4e-message-at-point :no-error))
      (let* ((tags (mu4e-message-field msg :tags))
             (remove (if (>= 1 (length tags))
                         (format "-%s" (car tags))
                       (->> (completing-read "Tag: " tags)
                            (s-split (rx (1+ " ")))
                            (--map (format "-%s" it))
                            (s-join ",")))))
        (mu4e-action-retag-message msg remove))
    (message "No message here")))

(defun std::mail::refresh (&optional arg)
  (interactive "P")
  (if (null arg)
      (mu4e-headers-rerun-search)
    (pfuture-callback ["mbsync" "-a"]
      :on-success
      (progn (mu4e-update-index)
             (mu4e-headers-rerun-search))
      :on-error (message "Mail Update failed: %s" (s-trim (pfuture-callback-output))))))
