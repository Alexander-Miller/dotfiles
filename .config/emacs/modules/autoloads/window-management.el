;; -*- lexical-binding: t -*-

(with-current-buffer (get-buffer-create "*Messages*")
  (evil-motion-state))

(defun std::kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun std::pop-to-messages-buffer ()
  "Pop to the messages buffer.
Delete it if it is shown already."
  (interactive)
  (-let [buf (messages-buffer)]
    (--if-let (get-buffer-window buf)
        (delete-window it)
      (with-current-buffer buf
        (goto-char (point-max))
        (pop-to-buffer (current-buffer))))))

(defun std::pop-to-compile-buffer ()
  (interactive)
  (-if-let (buf (--first
                 (eq 'compilation-mode (buffer-local-value 'major-mode it))
                 (buffer-list)))
    (--if-let (get-buffer-window buf)
        (delete-window it)
      (with-current-buffer buf
        (goto-char (point-max))
        (pop-to-buffer (current-buffer))))
    (message "No compilation buffers.")))
