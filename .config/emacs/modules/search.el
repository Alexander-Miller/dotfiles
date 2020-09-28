;; -*- lexical-binding: t -*-

(std::using-packages
 ctrlf
 helm-ag)

(std::autoload search
  #'std::search::rg-in-project)

(std::keybind
 :global
 "C-s"   #'ctrlf-forward-literal
 "C-M-S" #'ctrlf-forward-symbol-at-point
 :leader
 "/"  #'std::search::rg-in-project
 "sf" #'helm-do-ag-this-file
 "sd" #'helm-do-ag
 "sc" #'evil-ex-nohighlight)

;; Ctrlf Keybinds
(std::after ctrlf
  (setf ctrlf-mode-bindings
    '(("C-j"      . ctrlf-forward-literal)
      ("C-k"      . ctrlf-backward-literal)
      ("C-M-s"    . ctrlf-forward-symbol-at-point)
      ("<escape>" . ctrlf-cancel))))

;; Helm Ag Settings
(std::after helm-ag
  (setf helm-ag-base-command "rg --hidden --vimgrep --no-heading --smart-case"))
