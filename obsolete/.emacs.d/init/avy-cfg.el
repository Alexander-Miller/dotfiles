;;; avy-cfg.el --- avy config

;;; Commentary:
;;; Code:

(evil-define-motion evil-avy-goto-char (count)
  ""
  :type inclusive
  :repeat abort
  (evil-without-repeat
    (call-interactively 'avy-goto-char)))

(setq
 avy-all-windows      nil
 avy-background       nil
 avy-case-fold-search nil
 avy-goto-char-style  'at-full
 avy-goto-line-style  'at-full
 avy-highlight-first  t
 avy-keys             '(?a ?s ?d ?f ?q ?w ?e ?x ?c ?h ?j ?k ?l ?n ?m ?i)
 avy-timeout-seconds  4)

(a/def-key-for-maps
 (kbd "ü") #'avy-goto-char (remove evil-insert-state-map default-mode-maps))

(evil-leader/set-key
  "j" 'avy-goto-line)

(provide 'avy-cfg)
;;; avy-cfg.el ends here
