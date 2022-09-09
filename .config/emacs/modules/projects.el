;; -*- lexical-binding: t -*-

(std::using-packages
 multi-compile)

(std::autoload projects
  #'std::projects::hydra/body
  #'std::projects::multi-compile
  #'std::projects::compile-mode-hook
  #'std::projects::compile-filter-hook)

(std::keybind
 :leader
 "P"  #'std::projects::hydra/body
 "pf" #'project-find-file
 "pF" #'project-find-file-in
 "pr" #'project-query-replace-regexp
 "pc" #'std::projects::multi-compile
 "pC" #'recompile)

(std::after compile
  (setf
   compilation-scroll-output       'first-error
   multi-compile-completion-system 'default)

  (std::keybind
   :keymap compilation-mode-map
   "SPC" nil
   "g"   nil
   "gr"  #'recompile
   "C-." #'compilation-next-error
   "C-," #'compilation-previous-error)

  (add-hook 'compilation-filter-hook #'std::projects::compile-filter-hook)
  (add-hook 'compilation-mode-hook   #'std::projects::compile-mode-hook))

(std::add-hook 'makefile-mode-hook
  (setf company-backends
        '((company-capf company-files company-dabbrev-code company-keywords :with company-yasnippet))))
