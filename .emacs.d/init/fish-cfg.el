;;; fish-cfg.el --- fish config

;;; Commentary:
;;; Code:

(with-eval-after-load "fish-mode"

  (defvar shell-functions '())

  (defun create-shell-functions-list ()
    (setq shell-functions
          (sort (delete-dups (append (get-fish-functions) (get-path-functions))) 'string-lessp)))

  (defun get-fish-functions ()
    (split-string (shell-command-to-string "functions") "\n"))

  (defun get-path-functions ()
    (list-utils-flatten
     (map 'list (lambda (dir) (directory-files dir))
          (split-string (getenv "PATH") ":"))))

  (defun fetch-shell-functions ()
    (when (null shell-functions) (create-shell-functions-list))
    shell-functions)

  (defun get-doc-buffer (arg)
    (company-doc-buffer
     (let ((man-page (shell-command-to-string (format "man %s" arg))))
       (if (or
            (null man-page)
            (string-empty-p man-page)
            (string-prefix-p "No manual entry" man-page))
           (or
            (shell-command-to-string (format "%s --help" arg))
            (shell-command-to-string (format "%s -h" arg)))
         man-page))))

  (defun company-shell-functions (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (pcase command
      (`interactive (company-begin-backend 'company-shell-functions))
      (`prefix (company-grab-symbol))
      (`sorted t)
      (`duplicates nil)
      (`ignore-case nil)
      (`doc-buffer (get-doc-buffer arg))
      (`meta (nth 0 (split-string (shell-command-to-string (format "whatis %s" arg)) "\n")))
      (`candidates
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        (fetch-shell-functions)))))

  (defun fish-hook ()
    (setq-local company-backends '((company-shell-functions company-dabbrev-code company-files) company-dabbrev)))

  (add-hook 'fish-mode-hook #'fish-hook)

  (modify-syntax-entry ?~ "_" fish-mode-syntax-table)

  (define-key fish-mode-map (kbd "C-c C-c") #'executable-interpret)
  (define-key fish-mode-map (kbd "C-M-x")   #'sh-execute-region)

  (evil-leader/set-key-for-mode 'fish-mode
    "<tab> i" #'fish_indent))

(provide 'fish-cfg)
;;; dired-cfg.el ends here
