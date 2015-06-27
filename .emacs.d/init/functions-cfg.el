;;; functions-cfg.el --- collection of general purpose custom functions

;;; Commentary:

;;; Code:

(defun my/def-key-for-maps (key cmd maps)
  "Set KEY to invoke CMD for all keymaps in MAPS."
  (mapcar (lambda (map) (define-key map key cmd)) maps))

(defun my/aggressive-indent-if ()
  "Activate aggressive indent mode unless the current major mode prevents it."
  (unless
      (member
       (buffer-local-value 'major-mode (current-buffer))
       '(bibtex-mode
         cider-repl-mode
         coffee-mode
         comint-mode
         conf-mode
         Custom-mode
         diff-mode
         dired-mode
         doc-view-mode
         dos-mode
         erc-mode
         jabber-chat-mode
         haml-mode
         haskell-mode
         image-mode
         makefile-mode
         makefile-gmake-mode
         minibuffer-inactive-mode
         netcmd-mode
         python-mode
         sass-mode
         slim-mode
         special-mode
         shell-mode
         snippet-mode
         eshell-mode
         tabulated-list-mode
         term-mode
         TeX-output-mode
         text-mode
         org-mode
         yaml-mode))
    (aggressive-indent-mode 1)))

(defun my/newline-and-indent ()
  "Will insert a new line in insert and normal states, with the position adjusted in the latter case."
  (interactive)
  (cond
   ((evil-normal-state-p)
    (progn
      (evil-append 1)
      (newline-and-indent)
      (evil-normal-state)))
   ((evil-insert-state-p)
    (newline-and-indent))))

(defun my/quick-backward ()
  "Quicker backward scrolling."
  (interactive)
  (setq current-prefix-arg 5)
  (call-interactively 'evil-previous-visual-line))

(defun my/quick-forward ()
  "Quicker forward scrolling."
  (interactive)
  (setq current-prefix-arg 5)
  (call-interactively 'evil-next-visual-line))

(defun my/helm-mini-below ()
  "Open helm-mini on the underside of the screen reguardless of current helm-split-window-default-side value."
  (interactive)
  (let ((original-value 'helm-split-window-default-side))
    (setq-default helm-split-window-default-side 'below)
    (helm-for-files)
    (setq-default helm-split-window-default-side 'original-value)))

(defun my/what-face (point)
  "Reveal face at POINT."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" point))))

(defun my/what-major-mode ()
  "Provides the exact name of the current major mode."
  (interactive) (message "%s" major-mode))

(defun my/switch-dict (dict)
  "Use DICT as Ispell dictionary."
  (message "Now using \"%s\" dictionary." dict)
  (setq-default ispell-dictionary dict)
  (if flyspell-mode
      (progn (flyspell-mode nil) (flyspell-mode t) (flyspell-buffer))))

(defun my/choose-dict ()
  "Use Helm to choose new Ispell dictionary."
  (interactive)
  (let ((dicts (ispell-valid-dictionary-list)))
    (helm :sources '((name       . "Choose new Ispell dictionary")
                     (candidates . dicts)
                     (action     . my/switch-dict)))))

(defun evil-half-cursor ()
  "Rewrite of evil's own function.
Will remove calls to redisplay that render ace modes unbearably slow.
See: https://bitbucket.org/lyro/evil/issue/472/evil-half-cursor-makes-evil-ace-jump-mode"
(let (height)
  ;; make `window-line-height' reliable
  (setq height (window-line-height))
  (setq height (+ (nth 0 height) (nth 3 height)))
  ;; cut cursor height in half
  (setq height (/ height 2))
  (setq cursor-type (cons 'hbar height))
  ;; ensure the cursor is redisplayed
  (force-window-update (selected-window))))

(provide 'functions-cfg)
;;; functions-cfg.el ends here
