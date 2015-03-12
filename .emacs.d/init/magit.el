;; ============================
;; magit/git and its eco system
;; ============================

(with-eval-after-load 'magit
  (my/def-key-for-maps
   (kbd "C-k") 'magit-discard-item (list magit-status-mode-map))
  (my/def-key-for-maps
   (kbd "j") 'next-line (list magit-status-mode-map))
  (my/def-key-for-maps
   (kbd "k") 'previous-line (list magit-status-mode-map))
  (my/def-key-for-maps
   (kbd "M-j") 'magit-goto-next-sibling-section (list magit-status-mode-map))
  (my/def-key-for-maps
   (kbd "M-k") 'magit-goto-previous-sibling-section (list magit-status-mode-map)))


