;;; functions-cfg.el --- collection of general purpose custom functions

;;; Commentary:

;;; Code:

(defun a/def-key-for-maps (key cmd keymaps)
  "Bind KEY to CMD for all keymaps in MAPS."
  (dolist (keymap keymaps)
    (define-key keymap key cmd)))

(defun a/kill-delete-buffer ()
  "Kill current buffer and delete the window that held it, if possible."
  (interactive)
  (kill-this-buffer)
  (ignore-errors (delete-window)))

(defun a/newline-and-indent ()
  "Will insert a new line in insert and normal states, with the position adjusted in the latter case."
  (interactive)
  ;; (when (and (not (eq (point) (point-at-eol)))
             ;; (evil-normal-state-p))
    ;; (evil-forward-char))
  (newline-and-indent))

(defun a/quick-forward ()
  (interactive) (evil-next-visual-line 5))

(defun a/quick-backward ()
  (interactive) (evil-previous-visual-line 5))

(defun a/what-face (point)
  "Reveal face at POINT."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" point))))

(defun a/what-major-mode ()
  "Provides the exact name of the current major mode."
  (interactive) (message "%s" major-mode))

(defun a/switch-dict (dict)
  "Use DICT as Ispell dictionary."
  (message "Now using \"%s\" dictionary." dict)
  (setq ispell-dictionary dict)
  (if flyspell-mode
      (progn (flyspell-mode nil) (flyspell-mode t) (flyspell-buffer))))

(defun a/choose-dict ()
  "Use Helm to choose new Ispell dictionary."
  (interactive)
  (let ((dicts (ispell-valid-dictionary-list)))
    (helm :sources '((name       . "Choose new Ispell dictionary")
                     (candidates . dicts)
                     (action     . a/switch-dict)))))

(defun a/eval-last-sexp (eval-func)
  "Eval sexp with EVAL-FUNC compatible with evil normal state."
  (if (evil-normal-state-p)
      (save-excursion
        (if (eq (point) (point-at-eol))
            (evil-insert 1)
          (evil-append 1))
        (call-interactively eval-func)
        (evil-normal-state))
    (call-interactively eval-func)))

(defun a/vimish-fold-dwim ()
  "Toggle fold, or create on if it does not exist."
  (interactive)
  (ignore-errors (or (vimish-fold-toggle)
                     (call-interactively 'vimish-fold))))

(defun a/reload-config ()
  "Force reload of every single config file."
  (interactive)
  (dolist (f (directory-files "~/.emacs.d/init" t ".el$"))
    (load-file f)))

(defun a/reload-config-file ()
  "Force reload a single config file."
  (interactive)
  (helm :prompt "-> "
        :buffer "*helm reload cfg file*"
        :sources (helm-build-sync-source "Helm Reload Cfg File Source"
                   :candidates (-map
                                (lambda (f) (cons (file-name-base f)  f))
                                (directory-files "~/.emacs.d/init/" t ".el$"))
                   :action (lambda (f) (load-file f))
                   :persistent-action 'ignore
                   :filtered-candidate-transformer 'helm-fuzzy-highlight-matches)))

(provide 'functions-cfg)
;;; functions-cfg.el ends here
