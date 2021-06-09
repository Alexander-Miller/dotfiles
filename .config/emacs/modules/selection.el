;; -*- lexical-binding: t -*-

(std::using-packages
 helm
 helm-org
 selectrum
 prescient
 consult
 orderless
 selectrum-prescient
 marginalia
 avy
 link-hint)

(std::autoload selection
  #'std::selection::annotate-file
  #'std::selection::set-selectrum-candidates
  #'std::helm::org-in-buffer-headings
  #'std::helm::imenu)

(setf
 ;; completion
 completion-styles             '(orderless)
 completion-category-defaults  nil
 completion-category-overrides '((file (styles . (partial-completion))))

 ;; orderless
 orderless-skip-highlighting (lambda () selectrum-is-active)
 orderless-matching-styles   '(orderless-literal
                               orderless-initialism
                               orderless-regexp)

 ;; selectrum
 selectrum-prescient-enable-filtering         nil
 selectrum-highlight-candidates-function      #'orderless-highlight-matches
 selectrum-extend-current-candidate-highlight t

 ;; consult
 consult-preview-key nil

 ;;marginalia
 marginalia-align-offset 1

 ;; mini-frame
 mini-frame-resize          nil
 mini-frame-show-parameters #'std::mini-frame-show-parameters
 mini-frame-ignore-commands
 '(eval-expression
   "edebug-eval-expression"
   debugger-eval-expression
   ".*helm.*"
   "std::search"
   "std::help::manual"
   "std::org::inbox-refile-targets"))

(selectrum-mode)
(selectrum-prescient-mode)
(prescient-persist-mode)
(marginalia-mode)
(mini-frame-mode)

(defvar std::selectrum-candidates nil)

(std::add-advice #'std::selection::set-selectrum-candidates
    :before #'selectrum--read)

(defun std::mini-frame-show-parameters ()
  (let ((width 0.9)
        height)
    (pcase this-command
      ('consult-imenu (setf height 15 width 0.6))
      ('find-file     (setf height 10))
      ('find-library  (setf height 10))
      ((guard (not (null std::selectrum-candidates)))
       (setf height (if (listp std::selectrum-candidates)
                        (min 8 (1+ (length std::selectrum-candidates)))
                      8)))
      (_ (setf height 2)))
    (setf selectrum-max-window-height (1- height)
          std::selectrum-candidates nil)
    `((background-color . "#2E2E32")
      (left . 0.5)
      (top . 40)
      (height . ,height)
      (width . ,width))))

(std::pushnew marginalia-command-categories
  '(std::edit-module . short-file)
  '(std::org::goto-org-file . short-file))
(std::pushnew marginalia-annotator-registry
  '(short-file std::selection::annotate-file))

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
