;; -*- lexical-binding: t -*-

(std::using-packages
 avy
 consult
 link-hint
 marginalia
 orderless
 vertico)

(std::autoload selection
  #'std::selection::annotate-file
  #'std::selection::set-last-candidates
  #'std::selection::orderless-dispatcher
  #'std::selection::copy-candidate
  #'std::selection::files-up-one-level)

;; Miniframe
(mini-frame-mode)

(setf
 mini-frame-resize          nil
 mini-frame-show-parameters #'std::mini-frame-show-parameters
 mini-frame-ignore-commands
 '(eval-expression
   evil-ex
   which-key--show-keymap
   which-key--show-page
   "edebug-eval-expression"
   debugger-eval-expression
   ".*helm.*"
   "std::search"
   "std::help::manual"
   "std::org::inbox-refile-targets"))

(defvar std::selection::last-candidates nil)

(std::add-advice #'std::selection::set-last-candidates
    :before #'completing-read-default)
(std::add-advice #'std::selection::set-last-candidates
    :before #'completing-read-multiple)

(defun std::mini-frame-show-parameters ()
  (let ((width 0.9)
        (last-cs std::selection::last-candidates)
        height)
    (pcase this-command
      ('consult-imenu (setf height 15 width 0.6))
      ('find-file     (setf height 10))
      ('find-library  (setf height 10))
      ((guard last-cs)
       (setf height (if (listp last-cs)
                        (min 8 (1+ (length last-cs)))
                      8)))
      (_ (setf height 2)))
    (setf vertico-count (1- height)
          std::selection::last-candidates nil)
    `((background-color . "#2E2E32")
      (left . 0.5)
      (top . 40)
      (height . ,height)
      (width . ,width))))

;; Vertico
(vertico-mode)
(savehist-mode)

(setf
 completion-styles             '(orderless)
 completion-category-defaults  nil
 completion-category-overrides nil

 orderless-style-dispatchers '(std::selection::orderless-dispatcher)
 orderless-matching-styles   '(orderless-literal
                               orderless-initialism
                               orderless-prefixes)

 vertico-cycle           t)

;; Marginalia
(marginalia-mode)

(setf marginalia-align-offset 1)

(std::pushnew marginalia-command-categories
  '(std::buffers::edit-module    . short-file)
  '(std::buffers::edit-fish-file . short-file)
  '(std::buffers::edit-misc-cfg  . short-file)
  '(std::org::goto-org-file      . short-file))
(std::pushnew marginalia-annotator-registry
  '(short-file std::selection::annotate-file))

;; Consult
(setf consult-preview-key "M-,")

(std::after consult
  (dolist (cmd '(consult-outline
                 consult-mark
                 consult-global-mark
                 consult-imenu
                 consult-org-heading
                 consult-line))
    (evil-declare-not-repeat cmd)
    (evil-set-command-property cmd :jump t)))

;; Avy
(std::after avy
  (setf
   avy-keys             '(?a ?s ?d ?f ?q ?w ?e ?j ?k ?l ?o ?p)
   avy-all-windows      nil
   avy-case-fold-search nil))

;; Keys
(std::keybind
 :global
 "M-o" #'evil-avy-goto-char-timer
 "M-i" #'evil-avy-goto-word-1
 [remap list-buffers] #'consult-buffer
 [remap imenu]        #'consult-imenu
 [remap locate]       #'consult-locate
 :leader
 "ry" #'consult-yank-from-kill-ring
 "jf" #'find-function
 "jv" #'find-variable
 "jl" #'avy-goto-line
 "jk" #'link-hint-open-link
 "jy" #'link-hint-copy-link
 :keymap vertico-map
 "C-h" #'std::selection::files-up-one-level
 "C-j" #'vertico-next
 "M-j" #'vertico-next-group
 "C-k" #'vertico-previous
 "M-k" #'vertico-previous-group
 "M-c" #'std::selection::copy-candidate
 "<escape>" #'abort-minibuffers)
