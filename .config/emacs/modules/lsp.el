;; -*- lexical-binding: t -*-

(std::using-packages
 tree-sitter
 tree-sitter-langs
 lsp-mode
 lsp-ui
 lsp-treemacs)

(std::autoload lsp
  #'std::lsp::enable-tree-sitter-font-lock)

(add-hook 'python-mode-hook     #'std::lsp::enable-tree-sitter-font-lock)
(add-hook 'rust-mode-hook       #'std::lsp::enable-tree-sitter-font-lock)
(add-hook 'yaml-mode-hook       #'std::lsp::enable-tree-sitter-font-lock)
(add-hook 'js-mode-hook         #'std::lsp::enable-tree-sitter-font-lock)
(add-hook 'typescript-mode-hook #'std::lsp::enable-tree-sitter-font-lock)
(add-hook 'java-mode-hook       #'std::lsp::enable-tree-sitter-font-lock)

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
