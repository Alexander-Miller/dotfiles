;; -*- lexical-binding: t -*-

(std::using-packages
 eros
 macrostep)

(std::autoload elisp
  #'std::eval-last-sexp
  #'std::eval-defun)

(std::keybind
  :global
  [remap eval-last-sexp] #'std::eval-last-sexp
  :mode-leader emacs-lisp-mode
  "ee" #'std::eval-last-sexp
  "eb" #'eval-buffer
  "ef" #'std::eval-defun
  "dm" #'macrostep-mode
  :evil normal 'macrostep-mode
  "e" #'macrostep-expand
  "c" #'macrostep-collapse
  "q" #'macrostep-collapse-all)

(font-lock-add-keywords
   'emacs-lisp-mode
   `((,(rx (group-n
            1
            (not (any "#"))
            "'"
            symbol-start
            (1+ (or (syntax word)
                    (syntax symbol)))
            symbol-end))
      1 font-lock-type-face)
     (,(rx (group-n
            1
            "#'")
           (group-n
            2
            symbol-start
            (1+ (or (syntax word)
                    (syntax symbol)))
            symbol-end))
      (1 font-lock-constant-face)
      (2 font-lock-function-name-face)))
   'append)
