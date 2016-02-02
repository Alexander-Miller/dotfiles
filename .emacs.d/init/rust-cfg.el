;;; rust-cfg.el --- emacs rust configuration

;;; Commentary:
;;; Code:

(defun rust-hook ()
  "Rust mode hook."
  (racer-mode t)
  (flycheck-rust-setup))

(add-hook 'rust-mode-hook #'rust-hook)

(with-eval-after-load "racer"

  (evil-define-key 'normal racer-mode-map (kbd "M-.") 'racer-find-definition)
  (define-key racer-mode-map (kbd "C-c C-c") 'projectile-compile-project)

  (setq-default
   racer-cmd           "~/Documents/git/racer/target/release/racer"
   racer-rust-src-path "~/Documents/git/rust/src/"))

(provide 'rust-cfg)
;;; rust-cfg.el ends here
