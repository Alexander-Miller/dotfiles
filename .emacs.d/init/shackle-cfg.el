;;; shackle-cfg.el --- window behaviour settings

;;; Commentary:
;;; Code:

(shackle-mode t)

;; Order matters - first more specific, then more general rules
(setq shackle-rules
      '(("*helm-ag*"                :select   t :align right :size 0.5)
        ("*helm-mode-dired*"        :select   t :align below :size 0.3)
        ("*helm semantic/imenu*"    :select   t :align right :size 0.4)
        ("*Helm Find Files*"        :select   t :align below :size 0.3)
        ("*helm reload cfg file*"   :select   t :align below :size 0.3)
        ("*Helm man woman*"         :select   t :align below :size 0.3)
        ("*helm-mode-magit-status*" :select   t :align below :size 0.3)
        ("*Helm Completions*"       :select   t :align below :size 0.3)
        ("*Helm file completions*"  :select   t :align below :size 0.3)
        (".*helm.*"                 :regexp   t :align below :size 0.5)
        (rtags-mode                 :select   t :align below :size 0.5)
        (pdf-outline-buffer-mode    :select   t :align right :size 0.33)
        (flycheck-error-list-mode   :noselect t :align below :size 0.33)
        (Man-mode                   :select   t :align right :size 0.5)
        (magit-status-mode          :same     t)
        (magit-diff-mode            :noselect t :align right :size 0.5)
        (compilation-mode           :select   t :align right :size 0.5)
        (help-mode                  :select   t :align right :size 0.5)
        (special-mode               :select   t :align right :size 0.5)))

(provide 'shackle-cfg)
;; shackle-cfg.el ends here
