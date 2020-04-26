;; -*- lexical-binding: t -*-

(std::using-packages
 helm
 helm-org)

(std::autoload selection
  #'std::helm::org-in-buffer-headings
  #'std::helm::semantic-or-imenu)

(defun std::load-helm (fn &rest args)
  (helm-mode)
  (advice-remove #'completing-read      #'std::load-helm)
  (advice-remove #'read-string          #'std::load-helm)
  (advice-remove #'read-buffer          #'std::load-helm)
  (advice-remove #'read-file-name       #'std::load-helm)
  (advice-remove #'read-directory-name  #'std::load-helm)
  (advice-remove #'read-number          #'std::load-helm)
  (advice-remove #'read-from-minibuffer #'std::load-helm)
  (apply fn args))

(std::advice-add #'std::load-helm :around
  (completing-read read-string read-buffer read-from-minibuffer
                   read-number read-file-name read-directory-name))

(std::keybind
 :global
 "M-x" #'helm-M-x
 :leader
 "ff" #'helm-find-files
 "fl" #'helm-locate-library
 "fL" #'helm-locate
 "bb" #'helm-mini
 "bi" #'std::helm::semantic-or-imenu
 "ry" #'helm-show-kill-ring
 "rr" #'helm-register
 "sr" #'helm-resume)

(std::after helm

  (require 'helm-config)
  (require 'framey-helm)

  (setf helm-move-to-line-cycle-in-source t
		helm-echo-input-in-header-line    t
        helm-imenu-delimiter              ": ")

  (std::after org
    (setf helm-org-format-outline-path t
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

(std::after helm-files
  (std::keybind
   :keymap (helm-find-files-map helm-read-file-map)
   "C-h" #'helm-find-files-up-one-level
   "C-l" #'helm-find-files-down-last-level))
