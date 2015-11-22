;;; fish-cfg.el --- fish config

;;; Commentary:
;;; Code:

(with-eval-after-load "fish-mode"

  (defvar shell-functions '())

  (defun create-shell-functions-list ()
    (let ((path-funcs (get-path-functions))
          (fish-funcs (get-fish-functions)))
      (setq shell-functions (sort (delete-dups (append path-funcs fish-funcs)) 'string-lessp))))

  (defun get-fish-functions ()
    (split-string (shell-command-to-string "functions") "\n"))

  (defun get-path-functions ()
    (list-utils-flatten
     (map 'list (lambda (dir) (directory-files dir))
          (split-string (getenv "PATH") ":"))))

  (defun fetch-shell-functions ()
    (when (null shell-functions) (create-shell-functions-list))
    shell-functions)

  (defun company-shell-functions (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-sample-backend))
      (prefix (company-grab-symbol))
      (candidates
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        (fetch-shell-functions)))))

  (defun fish-hook ()
    (setq-local company-backends '((company-shell-functions company-dabbrev-code company-files) company-dabbrev)))

  (add-hook 'fish-mode-hook #'fish-hook)

  (define-key fish-mode-map (kbd "C-c C-c") #'executable-interpret)
  (define-key fish-mode-map (kbd "C-M-x")   #'sh-execute-region)

  (evil-leader/set-key-for-mode 'fish-mode
    "<tab> i" #'fish_indent))

(provide 'fish-cfg)
;;; dired-cfg.el ends here
