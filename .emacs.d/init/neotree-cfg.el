;;; neotree-cfg.el --- neotree config

;;; Commentary:
;;; Code:

(setq
 neo-auto-indent-point       nil
 neo-banner-message          "*NeoTree*"
 neo-create-file-auto-open   t
 neo-dont-be-alone           nil
 neo-mode-line-type          'default
 neo-persist-show            t
 neo-show-header             nil
 neo-show-hidden-files       t
 neo-show-updir-line         t
 neo-theme                   'arrow
 neo-window-width            35
 neo-window-fixed-size       nil)

(evil-define-state neo
  "Neotree state"
  :cursor '(hbar . 0)
  :enable (motion)
  (hl-line-mode t)
  (setq cursor-type nil)
  (setq-local show-paren-mode nil)
  (setq-local
   mode-line-format
   '("%e" (:eval (spaceline--prepare
                  '(workspace-number major-mode) nil)))))

(evil-set-initial-state 'neotree-mode 'neo)

(defun a/neotree-projectile-toggle ()
  "Open neotree with projectile root as root dir, center on current file."
  (interactive)
  (save-selected-window
    (neotree-dir (projectile-project-root)))
  (save-selected-window
    (neotree-find buffer-file-name)
    (recenter 'top)))

(defun a/neotree-root-up ()
  "Workaround to go up a dir from any node, keeping current selection."
  (interactive)
  (let ((current-line (s-trim (call-interactively 'evil-yank-line))))
    (or (search-backward ".. (up a dir)" nil t)
        (search-forward  ".. (up a dir)" nil t))
    (beginning-of-line)
    (push-button)
    (search-forward current-line)))

(global-set-key [f8] #'neotree-projectile-toggle)

(evil-leader/set-key
  "n n" #'neotree-toggle)

(define-key evil-neo-state-map (kbd "TAB") #'neotree-enter)
(define-key evil-neo-state-map (kbd "q")   #'a/kill-delete-buffer)
(define-key evil-neo-state-map (kbd "u")   #'a/neotree-root-up)
(define-key evil-neo-state-map (kbd "r")   #'neotree-rename-node)
(define-key evil-neo-state-map (kbd "+")   #'neotree-create-node)
(define-key evil-neo-state-map (kbd "d")   #'neotree-delete-node)
(define-key evil-neo-state-map (kbd "RET") #'neotree-change-root)
(define-key evil-neo-state-map (kbd "O")   #'ace-window)

(provide 'neotree-cfg)
;;; neotree-cfg.el ends here
