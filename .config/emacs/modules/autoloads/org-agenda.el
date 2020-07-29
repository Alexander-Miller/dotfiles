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
