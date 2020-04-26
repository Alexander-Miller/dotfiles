;; -*- lexical-binding: t -*-

(defun std::edit::fill-dwim ()
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively #'fill-region))
   ((eq major-mode 'org-mode)
    (call-interactively #'org-fill-paragraph))
   (t
    (call-interactively #'fill-paragraph))))

(defun std::edit::defun-query-replace (&optional arg)
  (interactive "P")
  (if arg
      (progn (mark-defun)
             (call-interactively 'anzu-query-replace))
    (anzu-query-replace-at-cursor-thing)))

(defun std::edit::fold-defun ()
  (interactive)
  (let* ((range (std::evil::defun-object))
         (beg (car range))
         (end (cadr range)))
    (vimish-fold beg end)))

(defun std::edit::fold-list ()
  (interactive)
  (require 'expand-region)
  (er/mark-outside-pairs)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (deactivate-mark)
    (vimish-fold beg end)))
