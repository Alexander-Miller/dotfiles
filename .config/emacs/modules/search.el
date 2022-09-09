;; -*- lexical-binding: t -*-

(std::using-packages
 ctrlf
 helm
 helm-ag
 helm-org)

(std::autoload search
  #'std::search::rg-in-project)

(std::keybind
 :global
 "C-s"   #'ctrlf-forward-literal
 "C-M-S" #'ctrlf-forward-symbol-at-point
 :leader
 "/"  #'std::search::rg-in-project
 "sr" #'helm-resume
 "sf" #'helm-do-ag-this-file
 "sd" #'helm-do-ag
 "sc" #'evil-ex-nohighlight)

(std::after ctrlf

  (std::keybind
   :keymap ctrlf-mode-map
   "C-j"   #'ctrlf-forward-literal
   "C-k"   #'ctrlf-backward-literal
   "C-M-s" #'ctrlf-forward-symbol-at-point
   "<escape>" #'ctrlf-cancel))

(std::after helm

  (require 'helm-config)
  (require 'framey-helm)

  (setf
   helm-move-to-line-cycle-in-source t
   helm-echo-input-in-header-line    t
   helm-imenu-delimiter              ": ")

  (std::after org
    (setf
     helm-org-format-outline-path t
     helm-org-headings-fontify t
     helm-org-headings-actions
     '(("Go to heading" . helm-org-goto-marker)
       ("Open in indirect buffer `C-c i'" . helm-org--open-heading-in-indirect-buffer)
       ("Refile heading(s) (marked-to-selected|current-to-selected) `C-c w`" . helm-org--refile-heading-to)
       ("Insert link to this heading `C-c l`" . helm-org-insert-link-to-heading-at-marker))) )

  (std::keybind
   :keymap helm-map
   "C-j" #'helm-next-line
   "C-k" #'helm-previous-line
   "M-j" #'helm-next-source
   "M-k" #'helm-previous-source
   "TAB" #'helm-execute-persistent-action
   "C-ö" #'helm-select-action
   "<escape>" #'helm-keyboard-quit))

(std::after helm-ag
  (setf
   helm-ag-base-command
   "rg --hidden --vimgrep --no-heading --smart-case"))
