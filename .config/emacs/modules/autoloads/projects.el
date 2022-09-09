;; -*- lexical-binding: t -*-

(require 'multi-compile)

(defun std::projects::compile-filter-hook ()
  "Apply ansi colors to compilation output."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun std::projects::compile-mode-hook ()
  (face-remap-add-relative 'default '((:foreground "#CCB18b"))))

(defun std::projects::multi-compile ()
  (interactive)
  (let* ((default-directory
           (or (-some-> (project-current)
                 (project-root))
               default-directory))
         (multi-compile-alist
          (append multi-compile-alist
                  (std::projects::get-make-targets))))
    (multi-compile-run)))

(defun std::projects::get-make-targets ()
  (when (file-exists-p "Makefile")
    (-let [cores (or
                  (when (file-exists-p "/proc/cpuinfo")
                    (with-temp-buffer
                      (insert-file-contents "/proc/cpuinfo")
                      (how-many "^processor[[:space:]]+:")))
                  1)]
      (list
       (cons
        "\\.*"
        (->> "cat Makefile | rg .PHONY: | cut -d ' ' -f 1-999"
             (shell-command-to-string)
             (s-trim-right)
             (s-split " ")
             (cdr)
             (--reject (s-starts-with? "." it))
             (--map (cons (capitalize it) (format "make %s -j%s" it cores)))))))))

(defhydra std::projects::hydra (:exit t :hint t)
  ("pf" #'project-find-file            "Find File")
  ("pF" #'project-find-file-in         "Find File In")
  ("pr" #'project-query-replace-regexp "Query Replace")
  ("pc" #'std::projects::multi-compile "Multi-Compile")
  ("pC" #'recompile                    "Recompile"))
