;; -*- lexical-binding: t -*-

(std::using-packages
 fish-mode
 company-shell
 vterm)

(std::autoload shell
  #'std::fish::mode-hook)

(add-hook 'fish-mode-hook #'std::fish::mode-hook)

(std::after fish-mode
  (setf
   company-shell-delete-duplicates nil
   company-shell-modes             nil
   company-fish-shell-modes        nil
   company-shell-use-help-arg      t))
