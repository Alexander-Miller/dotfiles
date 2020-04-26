;; -*- lexical-binding: t -*-

(defconst std::fish::imenu-expr
  (list
   (list
    "Function"
    (rx (group-n 1 (seq bol "function" (1+ space)))
        (group-n 2 (1+ (or alnum (syntax symbol)))) symbol-end)
    2)

   (list
    "Variables"
    (rx bol "set" (1+ space) (0+ "-" (1+ alpha) (1+ space))
        (group-n 1 symbol-start (1+ (or word "_"))))
    1)))

(defun std::fish::mode-hook ()
  (setf imenu-generic-expression std::fish::imenu-expr)
  (setq-local company-backends
              '((company-shell company-fish-shell company-shell-env company-capf company-files company-dabbrev-code :with company-yasnippet)
                (company-dabbrev company-dabbrev-code company-keywords))))
