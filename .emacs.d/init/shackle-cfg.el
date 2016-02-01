;;; shackle-cfg.el --- window behaviour settings

;;; Commentary:
;;; Code:

(shackle-mode t)

;; Order matters - first more specific, then more general rules
(setq-default shackle-rules
              '((help-mode                 :select   t :align right :size 0.5)
                (flycheck-error-list-mode  :noselect t :align below :size 0.33)
                (magit-diff-mode           :noselect t :align right :size 0.5)
                ("*helm-ag*"               :select   t :align right :size 0.5)
                ("*helm semantic/imenu*"   :select   t :align right :size 0.4)
                ("*Helm Find Files*"       :select   t :align below :size 0.3)
                ("*Helm Completions*"      :select   t :align below :size 0.3)
                ("*Helm file completions*" :select   t :align below :size 0.3)
                (".*helm.*"                :regexp   t :align below :size 0.5)))

(provide 'shackle-cfg)
;; shackle-cfg.el ends here
