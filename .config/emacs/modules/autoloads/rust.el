;; -*- lexical-binding: t -*-

(defun std::rust::mode-hook ()
  (lsp)
  (add-hook 'before-save-hook #'lsp-format-buffer nil :local))
