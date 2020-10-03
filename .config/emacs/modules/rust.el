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

(std::after multi-compile
  (std::pushnew multi-compile-alist
    '((memq major-mode '(rust-mode toml-mode))
      ("Debug Build"   "cargo build"           (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Release Build" "cargo build --release" (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Debug Run"     "cargo run"             (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Release Run"   "cargo run --release"   (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Cargo check"   "cargo check"           (locate-dominating-file (buffer-file-name) "Cargo.toml")))))
