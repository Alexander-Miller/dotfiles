;; -*- lexical-binding: t -*-

(autoload #'eros-eval-last-sexp "eros")

(defun std::elisp::eval-last-sexp ()
  "Eval the last sexp before point."
  (interactive)
  (save-excursion
    (when (and (evil-normal-state-p)
	           (string= ")" (string (or (char-after) 0))))
	  (forward-char))
    (call-interactively #'eros-eval-last-sexp)))

(defun std::elisp::eval-buffer ()
  (interactive)
  (eval-buffer)
  (eros--eval-overlay
   (format "Evaluated %S" (current-buffer))
   (point)))
