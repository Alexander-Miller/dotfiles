;; -*- lexical-binding: t -*-

(defconst std::shell::fish-imenu-expr
  (list
   (list
    "Function"
    (rx (group-n 1 (seq bol "function" (1+ space)))
        (group-n 2 (1+ (or alnum (syntax symbol)))) symbol-end)
    2)

   (list
    "Variables"
    (rx bol "set" (1+ space) (0+ "-" (1+ alpha) (1+ space))
        (group-n 1 symbol-start (1+ (or word "_"))))
    1)))

(defun std::shell::fish-mode-hook ()
  (setf imenu-generic-expression std::shell::fish-imenu-expr)
  (setq-local
   fish-indent-offset 2
   company-backends
   '((:separate
      company-shell company-fish-shell
      company-shell-env company-capf
      company-files company-dabbrev-code
      :with company-yasnippet)
     (:separate
      company-dabbrev company-dabbrev-code
      company-keywords))))

(defun std::shell::vterm ()
  "Pop to or create vterm buffer."
  (interactive)
  (require 'vterm)
  (let ((buffer (get-buffer-create "*vterm*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)))
    (pop-to-buffer buffer)))

(defun std::shell::kill-vterm-window-on-exit (_buffer _event)
  "Delete the vterm window on exit."
  (-when-let (w (--first (eq 'vterm-mode (->> it (window-buffer) (buffer-local-value 'major-mode)))
                         (window-list)))
    (kill-buffer (window-buffer w))
    (unless (one-window-p) (delete-window w))))
