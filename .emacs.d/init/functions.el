;; ========================
;; my own custom functions
;; ========================

(defun my/def-key-for-maps (key cmd maps)
  "Define key binding for all given key-maps."
  (mapcar (lambda (map) (define-key map key cmd)) maps))

(defun my/aggressive-indent-if ()
  "Activate aggressive indent mode unless the current major mode prevents it."
  (unless (member (buffer-local-value 'major-mode (current-buffer)) '(python-mode org-mode conf-space-mode))
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
  "Quicker forward scrolling"
  (interactive)
  (setq current-prefix-arg 5)
  (call-interactively 'evil-next-visual-line))

(defun my/helm-mini-below ()
  "Open helm-mini on the underside of the screen reguardless of current helm-split-window-default-side value."
  (interactive)
  (let ((org-value 'helm-split-window-default-side))
    (setq helm-split-window-default-side 'below)
    (helm-mini)
    (setq helm-split-window-default-side 'org-value)))

(defun my/what-face (pos)
  "Reveal face at point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun evil-half-cursor ()
  "Rewrite of evil's own function - will remove calls to redisplay that render ace modes unbearably slow.
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
