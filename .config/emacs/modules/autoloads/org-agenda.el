;; -*- lexical-binding: t -*-

(defun std::org-agenda::goto-today ()
  (interactive)
  (evil-goto-line)
  (std::schedule 0 :no-repeat
    (org-agenda-goto-today)))

(defun std::org-agenda::switch-to ()
  (interactive)
  (eyebrowse-switch-to-window-config (get #'org-agenda 'std::return-to-desktop))
  (org-agenda-switch-to))

(defun std::org::agenda::mark-habits ()
  "https://emacs.stackexchange.com/a/17328/16972"
  (when (not (get-text-property (point) 'org-series))
    (let ((cursor (point))
          (item)
          (data))
      (while (setf cursor (next-single-property-change cursor 'org-marker))
        (setf item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setf data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))
