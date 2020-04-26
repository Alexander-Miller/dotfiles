;; -*- lexical-binding: t -*-

(defun std::help::pacman-pkg-info ()
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
