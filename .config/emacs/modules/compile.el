;; -*- lexical-binding: t -*-

(std::using-packages
 multi-compile)

(std::autoload compile
  #'std::multi-compile
  #'std::compile::mode-hook
  #'std::compile::filter-hook)

(std::keybind
 :leader
 "pc" #'std::multi-compile
 "pC" #'recompile)

(std::after compile
  (setf
   compilation-scroll-output       'first-error
   multi-compile-completion-system 'helm
   multi-compile-alist
   '((rust-mode
      ("Debug Build"   "cargo build"           (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Release Build" "cargo build --release" (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Debug Run"     "cargo run"             (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      ("Release Run"   "cargo run --release"   (locate-dominating-file (buffer-file-name) "Cargo.toml")))))

  (std::keybind
   :keymap compilation-mode-map
   "SPC" #'evil-send-leader
   "C-." #'compilation-next-error
   "C-," #'compilation-previous-error)

  (add-hook 'compilation-filter-hook #'std::compile::filter-hook)
  (add-hook 'compilation-mode-hook #'std::compile::mode-hook))

(std::add-hook 'makefile-mode-hook
  (setf company-backends
        '((company-capf company-files company-dabbrev-code company-keywords :with company-yasnippet))))
