;; -*- lexical-binding: t -*-

(defun std::org-agenda::goto-today ()
  (interactive)
  (evil-goto-line)
  (std::schedule 0 :no-repeat
    (org-agenda-goto-today)))
