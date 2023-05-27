;; -*- lexical-binding: t -*-

(evil-set-initial-state 'helpful-mode 'motion)

(defun std::help::pacman-info ()
  "System package info based on `pacman -Qi $pkg'."
  (interactive)
  (let* ((completions
          (->> "pacman -Q"
               (shell-command-to-string)
               (s-trim)
               (s-lines)
               (--map (car (s-split " " it :no-nulls)))))
         (name (completing-read "Package: " completions)))
    (switch-to-buffer (get-buffer-create "*Package Info*"))
    (erase-buffer)
    (-> (format "pacman -Qi %s" name)
        (shell-command-to-string)
        (s-trim)
        (insert))
    (goto-char 0)
    (conf-mode)))

(defhydra std::help::hydra (:exit t :hint t)
  ("i" #'std::help::manual-info "Manual")
  ("v" #'helpful-variable       "Variables")
  ("f" #'helpful-callable       "Functions")
  ("k" #'helpful-key            "Keybinds")
  ("c" #'describe-char          "Char-at-Point")
  ("C" #'helpful-command        "Commands")
  ("F" #'describe-face          "Faces")
  ("p" #'epkg-describe-package  "Packages")
  ("P" #'std::help::pacman-info "System Packages")
  ("m" #'man                    "Man"))
