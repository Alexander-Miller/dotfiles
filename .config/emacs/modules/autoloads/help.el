;; -*- lexical-binding: t -*-

(require 'helm-info)

(defun std::help::pacman-info ()
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

(defun std::help::manual-info ()
  (interactive)
  (helm :sources '(helm-source-info-emacs
                   helm-source-info-elisp
                   helm-source-info-cl)))

(defun std::help::hydra ()
  (interactive)
  (std::help::hydra/body))

(defhydra std::help::hydra (:exit t :hint t)
  ("i" #'std::help::manual-info "Manual")
  ("v" #'helpful-variable       "Variables")
  ("f" #'helpful-callable       "Functions")
  ("k" #'helpful-key            "Keybinds")
  ("c" #'describe-char          "Char-at-Point")
  ("C" #'helpful-command        "Commands")
  ("F" #'describe-face          "Faces")
  ("a" #'helm-apropos           "Apropos")
  ("p" #'epkg-describe-package  "Packages")
  ("P" #'std::help::pacman-info "System Packages")
  ("m" #'helm-man-woman         "Man"))
