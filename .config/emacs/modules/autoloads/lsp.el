;; -*- lexical-binding: t -*-

(defun std::lsp::enable-tree-sitter-font-lock ()
  (require 'tree-sitter-langs)
  (tree-sitter-mode)
  (tree-sitter-hl-mode))
