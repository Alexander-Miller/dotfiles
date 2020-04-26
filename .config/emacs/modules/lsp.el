;; -*- lexical-binding: t -*-

(std::using-packages
 lsp-mode
 lsp-ui
 lsp-treemacs)

(std::after lsp-mode
  (setf lsp-ui-flycheck-live-reporting nil))
