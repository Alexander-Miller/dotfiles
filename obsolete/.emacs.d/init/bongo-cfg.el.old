;;; bongo-cfg.el --- bongo player config

;;; Commentary:
;;; Code:

(require 'bongo)
(add-to-list 'auto-mode-alist '("\\.bongo-playlist\\'" . bongo-playlist-mode))
(add-to-list 'auto-mode-alist '("\\.bongo-library\\'"  . bongo-library-mode))

(evil-leader/set-key "t m" #'bongo)

(with-eval-after-load "bongo"

  (setq-default
   bongo-confirm-flush-playlist             t
   bongo-default-directory                  "~/Music/"
   bongo-display-inline-playback-progress   t
   bongo-display-playlist-after-enqueue     t
   bongo-dnd-support                        nil
   bongo-insert-whole-directory-trees       t
   bongo-mark-played-tracks                 nil
   bongo-update-references-to-renamed-files t)

  (evil-define-key 'normal bongo-mode-map (kbd "<tab>") 'bongo-dwim)
  (evil-define-key 'normal bongo-mode-map (kbd "RET")   'bongo-dwim)
  (evil-define-key 'normal bongo-mode-map (kbd ",")     'bongo-switch-buffers)
  (evil-define-key 'normal bongo-mode-map (kbd "C-k")   'bongo-kill)
  (evil-define-key 'normal bongo-mode-map (kbd "C-q")   'bongo-quit)
  (evil-define-key 'normal bongo-mode-map (kbd "E")     'bongo-insert-enqueue)
  (evil-define-key 'normal bongo-mode-map (kbd "e")     'bongo-append-enqueue)
  (evil-define-key 'normal bongo-mode-map (kbd "p")     'bongo-pause/resume)
  (evil-define-key 'normal bongo-mode-map (kbd "f")     'bongo-seek)
  (evil-define-key 'normal bongo-mode-map (kbd "s")     'bongo-start/stop)
  (evil-define-key 'normal bongo-mode-map (kbd "m")     'bongo-mark-forward)
  (evil-define-key 'normal bongo-mode-map (kbd "u")     'bongo-unmark-forward)

  (evil-define-key 'normal bongo-library-mode-map (kbd "i") 'bongo-insert-file)
  )

(provide 'bongo-cfg)
;;; bongo-cfg.el ends here
