;;; rust-cfg.el --- emacs rust configuration

;;; Commentary:
;;; Code:

(with-eval-after-load "rust-mode"

  (define-key racer-mode-map (kbd "M-,")     'racer-find-definition)
  (define-key racer-mode-map (kbd "C-c C-c") 'projectile-compile-project)

  (defun rust-hook ()
    "Rust mode hook."
    (racer-mode t)
    (prettify-symbols-mode t))

  (add-hook 'rust-mode-hook #'rust-hook)

  (setq-default
   racer-cmd           "~/Documents/git/racer/target/release/racer"
   racer-rust-src-path "~/Documents/git/rust/src/"))

(provide 'rust-cfg)
;;; rust-cfg.el ends here
