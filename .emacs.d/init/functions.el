;; ========================
;; my own custom functions
;; ========================

(defun my/def-key-for-maps (key cmd maps)
  "Define key binding for all given key-maps."
  (mapcar (lambda (map) (define-key map key cmd)) maps))

(defun my/aggressive-indent-if ()
  "Activate aggressive indent mode unless the current major mode prevents it."
  (unless (member (buffer-local-value 'major-mode (current-buffer)) '(python-mode org-mode))
    (aggressive-indent-mode 1)))
