;;; pdf-cfg.el --- pdf config

;;; Commentary:
;;; Code:

(pdf-tools-install)

(with-eval-after-load 'pdf-view

  (setq
   pdf-view-continuous nil)

  (evil-define-key 'normal pdf-view-mode-map
    (kbd "C-s") #'isearch-forward
    (kbd "C-r") #'isearch-backward
    [escape]    #'keyboard-quit
    "h"  (lambda () (interactive) (image-backward-hscroll 3))
    "l"  (lambda () (interactive) (image-forward-hscroll 3))
    "j"  (lambda () (interactive) (pdf-view-scroll-up-or-next-page 3))
    "k"  (lambda () (interactive) (pdf-view-scroll-down-or-previous-page 3))
    "J"  (lambda () (interactive) (pdf-view-scroll-up-or-next-page 10))
    "K"  (lambda () (interactive) (pdf-view-scroll-down-or-previous-page 10))
    "n"  (lambda () (interactive) (pdf-view-next-page) (image-bob))
    "p"  (lambda () (interactive) (pdf-view-previous-page) (image-eob))
    "q"  #'quit-window
    "gr" #'revert-buffer
    "gg" #'image-bob
    "G"  #'image-eob
    "<"  #'pdf-view-first-page
    ">"  #'pdf-view-last-page
    "O"  #'pdf-occur
    "d"  #'pdf-links-action-perform
    "F"  #'pdf-links-isearch-link
    "o"  #'pdf-outline
    "sb" #'pdf-view-set-slice-from-bounding-box
    "sr" #'pdf-view-reset-slice
    "+"  #'pdf-view-enlarge
    "-"  #'pdf-view-shrink
    "0"  #'pdf-view-scale-reset
    "H"  #'pdf-view-fit-height-to-window
    "W"  #'pdf-view-fit-width-to-window
    "P"  #'pdf-view-fit-page-to-window
    "m"  #'pdf-view-midnight-minor-mode)

  (evil-leader/set-key-for-mode 'pdf-view-mode
    "<tab> l" #'pdf-annot-list-annotations)

  (evil-define-state pdf-outline
    "Pdf Outline"
    :tag "<Outline>"
    :cursor (hbar . 0)
    :enable (normal)
    :suppress-keymap t
    (hl-line-mode t))

  (evil-set-initial-state 'pdf-outline-buffer-mode 'pdf-outline)

  (advice-add #'pdf-outline-follow-link-and-quit :after #'delete-other-windows)

  (evil-define-key 'pdf-outline pdf-outline-buffer-mode-map
    [escape]      #'keyboard-quit
    (kbd "<tab>") #'outline-toggle-children
    (kbd "<RET>") #'pdf-outline-follow-link-and-quit
    "h"           #'pdf-outline-display-link
    "l"           #'pdf-outline-follow-link
    "q"           #'pdf-outline-quit-and-kill))

(provide 'pdf-cfg)
;;; pdf-cfg.el ends here
