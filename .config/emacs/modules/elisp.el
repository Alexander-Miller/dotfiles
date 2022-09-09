;; -*- lexical-binding: t -*-

(std::using-packages
 eros
 macrostep
 cask-mode)

(std::autoload elisp #'std::elisp::eval-last-sexp)

(autoload #'eros-eval-defun "eros")

(std::keybind
  :global
  [remap eval-last-sexp] #'std::elisp::eval-last-sexp
  :mode-leader emacs-lisp-mode
  "ee" #'std::eval-last-sexp
  "eb" #'eval-buffer
  "ef" #'eros-eval-defun
  "dm" #'macrostep-mode
  "df" #'edebug-defun
  :evil (normal motion) 'edebug-mode
  "n" #'edebug-next-mode
  "s" #'edebug-set-breakpoint
  "S" #'edebug-unset-breakpoint
  "b" #'edebug-next-breakpoint
  "c" #'edebug-continue-mode
  "q" #'top-level
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
