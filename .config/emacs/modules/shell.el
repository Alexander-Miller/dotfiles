;; -*- lexical-binding: t -*-

(std::using-packages
 fish-mode
 company-shell
 vterm)

(std::autoload shell
  #'std::shell::fish-mode-hook
  #'std::shell::vterm
  #'std::shell::kill-vterm-window-on-exit)

(std::keybind
 :leader
 "bt" #'std::shell::vterm)

(add-hook 'fish-mode-hook #'std::shell::fish-mode-hook)

(std::after fish-mode
  (setf
   company-shell-delete-duplicates nil
   company-shell-modes             nil
   company-fish-shell-modes        nil
   company-shell-use-help-arg      t))

(std::after vterm

  (evil-set-initial-state 'vterm-mode 'emacs)

  (add-hook 'vterm-exit-functions #'std::shell::kill-vterm-window-on-exit))
