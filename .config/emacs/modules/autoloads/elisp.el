;; -*- lexical-binding: t -*-

(autoload #'eros-eval-last-sexp "eros")
(autoload #'eros-eval-defun "eros")

(defun std::eval-last-sexp ()
  (interactive)
  (let ((func #'eros-eval-last-sexp))
    (if (and (evil-normal-state-p)
	     (string= ")" (string (or (char-after) 0))))
	(save-excursion
	  (forward-char)
	  (call-interactively func))
      (call-interactively func))))

(defun std::eval-defun (arg)
  (interactive "P")
  (eros-eval-defun arg))
