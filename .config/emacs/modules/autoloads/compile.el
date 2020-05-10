;; -*- lexical-binding: t -*-

(defun std::compile::ansify ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun std::compile::mode-hook ()
  (face-remap-add-relative 'default '((:foreground "#CCB18b"))))

(defun std::multi-compile ()
  (interactive)
  (let* ((default-directory
           (or (projectile-project-root)
               default-directory))
        (multi-compile-alist
         (append multi-compile-alist
                 (std::compile::get-make-targets))))
    (call-interactively #'multi-compile-run)))

(defun std::compile::get-make-targets ()
  (when (file-exists-p "Makefile")
    (list
     (cons
      "\\.*"
      (->> "cat Makefile | rg .PHONY: | cut -d ' ' -f 1-999"
           (shell-command-to-string)
           (s-trim-right)
           (s-split " ")
           (cdr)
           (--reject (s-starts-with? "." it))
           (--map (cons (capitalize it) (format "make %s" it))))))))
