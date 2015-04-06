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

(defun my/quick-forward ()
  "Quicker forward scrolling"
  (interactive)
  (setq current-prefix-arg 5)
  (call-interactively 'evil-next-visual-line))

(defun my/quick-backward ()
  "Quicker backward scrolling"
  (interactive)
  (setq current-prefix-arg 5)
  (call-interactively 'evil-previous-visual-line))

(defun evil-half-cursor ()
  "Rewrite of evil's own function - removes calls to redisplay that render ace modes unbearably slow.
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
