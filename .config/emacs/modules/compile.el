;; -*- lexical-binding: t -*-

(std::using-packages
 multi-compile)

(std::autoload compile
  #'std::multi-compile
  #'std::compile::mode-hook
  #'std::comint-hook)

(std::keybind
 :leader
 "pc" #'std::multi-compile)

(std::after compile
  (setf
   multi-compile-completion-system 'helm)

  (add-hook 'compilation-filter-hook #'std::compile::ansify)
  (add-hook 'compilation-mode-hook #'std::compile::mode-hook))
