;; -*- lexical-binding: t -*-

(std::defun-with-desktop
 :name std::mu4e
 :command #'mu4e
 :check (memq major-mode '(mu4e-main-mode mu4e-view-mode mu4e-headers-mode mu4e-compose-mode)))

(defun std::mail::compose-mode-hook ()
  (use-hard-newlines -1)
  (setq-local
   company-backends
   '((company-capf std::completion::prose-complete  company-files company-dabbrev :with company-yasnippet))))

(defun std::mail::view-mode-hook ()
  (visual-line-mode)
  (setq-local face-remapping-alist '((avy-lead-face ((:inherit avy-lead-face :family "Fantasque Sans Mono"))))))

(defhydra std::mail::tag (:exit t :hint nil)
  ("t" (std::mail::do-tag "T") "T")
  ("w" (std::mail::do-tag "W") "W")
  ("x" (std::mail::do-tag "X") "X")
  ("q" nil "cancel"))

(defhydra std::mail::mark (:exit t :hint t)
  ("p"   #'std::mail::move-mail-to-in-progress "In Progress")
  ("m"   #'mu4e-headers-mark-for-move          "Move")
  ("!"   #'mu4e-headers-mark-for-read          "Read")
  ("\""  #'mu4e-headers-mark-for-unread        "Unread")
  ("ü"   #'mu4e-headers-mark-for-flag          "Flag")
  ("Ü"   #'mu4e-headers-mark-for-unflag        "Unflag")
  ("d"   #'mu4e-headers-mark-for-trash         "Trash")
  ("="   #'mu4e-headers-mark-for-trash         "Untrash")
  ("D"   #'mu4e-headers-mark-for-delete        "Delete")
  ("e"   #'mu4e-headers-mark-for-refile        "Refile"))

(defun std::mail::do-tag (tag)
  (interactive (list (format "+%s" (cfrs-read "Tag: "))))
  (-if-let (msg (mu4e-message-at-point :no-error))
      (-let [tags (mu4e-message-field msg :tags)]
        (if (member tag tags)
            (mu4e-action-retag-message msg (format "-%s" tag))
          (mu4e-action-retag-message msg (format "+%s" tag)) ))
    (user-error "No message here")))

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

(defun std::mail::capture-message ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "pm"))
