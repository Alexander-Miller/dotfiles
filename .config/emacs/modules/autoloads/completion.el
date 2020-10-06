;; -*- lexical-binding: t -*-

(defvar std::prose-complete-loaded nil)

(defun std::completion::prose-complete (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'std::completion::prose-complete))
    (prefix      (company-grab-symbol))
    (sorted      t)
    (duplicates  nil)
    (ignore-case nil)
    (no-cache    nil)
    (annotation  nil)
    (candidates  (prose-complete-lookup arg))))

(-let [lib (concat std::repos-dir "/prose-complete/target/release/libprose_complete.so")]
  (if (file-exists-p lib)
      (unless std::prose-complete-loaded
        (module-load lib)
        (setf std::prose-complete-loaded t))
    (fset #'std::completion::prose-complete #'company-dabbrev)))

(defun std::completion::complete-and-keep-frontend ()
  (interactive)
  (company-complete-selection)
  (company-manual-begin))

(defun std::completion::prose-hook ()
  (setq-local
   company-idle-delay            0.25
   company-minimum-prefix-length 4
   company-backends
   '((std::completion::prose-complete company-capf company-files company-dabbrev :with company-yasnippet))))
