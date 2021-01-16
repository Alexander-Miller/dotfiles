;; -*- lexical-binding: t -*-

(std::using-packages
 lsp-mode
 lsp-ui
 lsp-treemacs)

(std::after lsp-mode
  (setf
   lsp-completion-provider :capf
   read-process-output-max (* 1024 1024)))

(std::after lsp-ui
  (setf
   lsp-ui-flycheck-live-reporting nil
   lsp-ui-doc-enable              nil))

(std::after lsp-mode
  (std::keybind
   :keymap lsp-mode-map
   "M-RET" #'lsp-execute-code-action
   "<f2>"  #'lsp-ui-doc-glance
   :leader
   "ldf" #'lsp-ui-doc-focus-frame
   "lrr" #'lsp-rename))
