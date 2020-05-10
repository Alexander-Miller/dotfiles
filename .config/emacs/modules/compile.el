;; -*- lexical-binding: t -*-

(std::using-packages
 multi-compile)

(std::autoload compile
  #'std::multi-compile
  #'std::compile::mode-hook
  #'std::compile::filter-hook)

(std::keybind
 :leader
 "pc" #'std::multi-compile)

(std::after compile
  (setf
   compilation-scroll-output       'first-error
   multi-compile-completion-system 'helm)

  (add-hook 'compilation-filter-hook #'std::compile::filter-hook)
  (add-hook 'compilation-mode-hook #'std::compile::mode-hook))
