;; -*- lexical-binding: t -*-

(std::using-packages
 toml-mode
 rust-mode)

(add-hook 'rust-mode-hook #'lsp)

(std::after lsp-mode
  (std::pushnew lsp-disabled-clients 'rls))

(std::after rust-mode
  (setf lsp-rust-server 'rust-analyzer))
