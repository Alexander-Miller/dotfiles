;; -*- lexical-binding: t -*-

(std::using-packages
 ctrlf
 link-hint
 helm-ag)

(std::autoload search
  #'std::search::rg-in-project)

(setf avy-all-windows      nil
      avy-case-fold-search nil
      ivy-height           4
      isearch-forward      t)

(std::keybind
 :global
 "C-s"   #'ctrlf-forward-literal
 "C-M-S" #'ctrlf-forward-symbol-at-point
 "M-o"   #'evil-avy-goto-char-2
 "M-O"   #'evil-avy-goto-char
 :leader
 "/"  #'std::search::rg-in-project
 "sf" #'helm-do-ag-this-file
 "sd" #'helm-do-ag
 "sc" #'evil-ex-nohighlight
 ;; jumping
 "jf" #'find-function
 "jl" #'avy-goto-line
 "jk" #'link-hint-open-link
 "jy" #'link-hint-copy-link)

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
