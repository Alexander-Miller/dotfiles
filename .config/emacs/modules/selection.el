;; -*- lexical-binding: t -*-

(std::using-packages
 helm
 helm-org
 selectrum
 prescient
 consult
 consult-selectrum
 selectrum-prescient
 marginalia
 avy
 link-hint)

(std::autoload selection
  #'std::helm::org-in-buffer-headings
  #'std::helm::imenu)

(selectrum-mode)
(selectrum-prescient-mode)
(marginalia-mode)
(mini-frame-mode)

(setf
 consult-preview-key        nil
 marginalia-align-offset    1
 mini-frame-resize          nil
 marginalia-annotators      '(marginalia-annotators-heavy nil marginalia-annotators-light)
 mini-frame-show-parameters #'std::mini-frame-show-parameters
 selectrum-extend-current-candidate-highlight
 t
 mini-frame-ignore-commands
 '(eval-expression
   "edebug-eval-expression"
   debugger-eval-expression
   ".*helm.*"
   "std::org::inbox-refile-targets"))

(defun std::mini-frame-show-parameters ()
  (let ((size-args
         (pcase this-command
           ('consult-imenu
            (setf selectrum-max-window-height 15)
            '((width . 0.6) (height . 15)))
           ('find-file
            (setf selectrum-max-window-height 10)
            '((width . 0.9) (height . 10)))
           ('find-library
            (setf selectrum-max-window-height 10)
            '((width . 0.9) (height . 10)))
           ((guard (not (null minibuffer-completion-table)))
            (-let [height (if (listp minibuffer-completion-table)
                              (min 8 (1+ (length minibuffer-completion-table)))
                            8)]
              (setf selectrum-max-window-height height)
              `((width . 0.9) (height . ,height))))
           (_
            (setf selectrum-max-window-height 2)
            '((width . 0.9) (height . 2))))))
    `((background-color . "#2E2E32") (left . 0.5) (top . 40) ,@size-args)))

(std::keybind
 :global
 "M-o" #'evil-avy-goto-char-timer
 "M-i" #'evil-avy-goto-word-1
 :leader
 "ff" #'find-file
 "fl" #'find-library
 "bb" #'purpose-switch-buffer-overload
 "bb" #'consult-buffer
 ;; "br" #'helm-recentf
 "bi" #'consult-imenu
 "ry" #'consult-yank
 ;; "rr" #'helm-register
 "sr" #'helm-resume
 "jf" #'find-function
 "jl" #'avy-goto-line
 "jk" #'link-hint-open-link
 "jy" #'link-hint-copy-link
 :keymap selectrum-minibuffer-map
 "C-j" #'selectrum-next-candidate
 "C-k" #'selectrum-previous-candidate
 "<escape>"   #'keyboard-quit
 "C-<return>" #'selectrum-submit-exact-input)

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

(std::after avy
  (setf
   avy-keys             '(?a ?s ?d ?f ?q ?w ?e ?j ?k ?l ?o ?p)
   avy-all-windows      nil
   avy-case-fold-search nil))
