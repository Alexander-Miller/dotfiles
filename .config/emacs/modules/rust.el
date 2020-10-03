;; -*- lexical-binding: t -*-

(std::using-packages
 toml-mode
 rust-mode)

(std::autoload rust
  #'std::rust::mode-hook)

(add-hook 'rust-mode-hook #'std::rust::mode-hook)

(std::after lsp-mode
  (std::pushnew lsp-disabled-clients 'rls))

(std::after rust-mode
  (setf
   rust-format-on-save nil
   lsp-rust-server     'rust-analyzer))
