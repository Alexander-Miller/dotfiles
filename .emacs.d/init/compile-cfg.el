;;; compile-cfg.el --- compilation config

;;; Commentary:
;;; Code:

(setq-default
 multi-compile-completion-system 'helm
 multi-compile-alist
 '((rust-mode   ("cargo run"           . "cargo run")
                ("cargo run release"   . "cargo run --release")
                ("cargo build"         . "cargo build")
                ("cargo build release" . "cargo build --release")
                ("cargo test"          . "cargo test"))

   (python-mode ("run2"                . "python2 %file-name")
                ("run3"                . "python3 %file-name"))

   (fish-mode   ("fish run"            . "fish %file-name"))

   (c++-mode    ("simple run"          . "g++ %file-name -o out -std=c++14; and ./out"))))

(with-eval-after-load "fish-mode"
  (define-key fish-mode-map (kbd "C-c C-c") #'multi-compile-run))
(with-eval-after-load "racer"
  (define-key racer-mode-map (kbd "C-c C-c") #'multi-compile-run))
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "C-c C-c") #'multi-compile-run))
(with-eval-after-load "cc-mode"
  (define-key c++-mode-map (kbd "C-c C-c") #'multi-compile-run))

(provide 'compile-cfg)
;;; compile-cfg.el ends here
