;; ======================
;; git and its eco system
;; ======================

(defun bind-key-for-maps (maps key action)
  (mapcar
   (lambda (map)
     (define-key map key action))
   maps))

(eval-after-load 'magit
  '(progn
     (define-key magit-status-mode-map (kbd "c-k") 'magit-discard-item)
     ;;(define-key magit-status-mode-map (kbd "j") 'next-line)
     ;;(define-key magit-status-mode-map (kbd "k") 'previous-line)
     (bind-key-for-maps (list magit-status-mode-map) (kbd "j") 'next-line)
     (bind-key-for-maps (list magit-status-mode-map) (kbd "k") 'previous-line)
     (bind-key-for-maps (list magit-status-mode-map) (kbd "M-j") 'magit-goto-next-sibling-section)
     (bind-key-for-maps (list magit-status-mode-map) (kbd "M-k") 'magit-goto-previous-sibling-section)))


