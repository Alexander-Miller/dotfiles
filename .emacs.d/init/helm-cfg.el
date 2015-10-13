;;; helm-cfg.el --- helm config

;;; Commentary:
;;; Code:

(helm-mode 1)

(setq-default
 helm-ag-base-command                   "ag -f --hidden --nocolor --nogroup --depth -1"
 helm-echo-input-in-header-line         t
 helm-M-x-fuzzy-match                   t
 helm-semantic-fuzzy-match              t
 helm-imenu-fuzzy-match                 t
 helm-apropos-fuzzy-match               t
 helm-buffers-fuzzy-matching            t
 helm-completion-in-region-fuzzy-match  t
 helm-file-cache-fuzzy-match            t
 helm-projectile-fuzzy-match            t
 helm-semantic-fuzzy-match              t
 helm-locate-fuzzy-match                nil
 helm-recentf-fuzzy-match               t
 helm-candidate-number-limit            100
 helm-move-to-line-cycle-in-source      t
 helm-scroll-amount                     5
 helm-ff-file-name-history-use-recentf  t
 helm-autoresize-mode                   t
 helm-autoresize-max-height             50
 helm-autoresize-min-height             50
 helm-adaptive-mode                     t
 helm-ff-search-library-in-sexp         t
 helm-quick-update                      t
 helm-idle-delay                        0.1
 helm-input-idle-delay                  0.1
 helm-display-source-at-screen-top      nil
 helm-completion-window-scroll-margin   5
 helm-split-window-default-side         'right
 helm-kill-ring-max-lines-number        5
 helm-default-external-file-browser     "urxvt -e ranger"
 helm-ff-transformer-show-only-basename t
 helm-ff-auto-update-initial-value      nil
 helm-for-files-preferred-list          '(helm-source-buffers-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-file-cache
                                          helm-source-files-in-current-dir
                                          helm-source-locate
                                          helm-source-buffer-not-found))

(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

;; flx for helm
;; https://github.com/PythonNut/emacs-config/blob/f1df3ac16410bfa72d88855325bd6c2de56f587b/modules/config-helm.el#L33#L89

(require 'flx)
;; this is a bit hackish, ATM, redefining functions I don't own
(defvar helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-str))

(defun helm-score-candidate-for-pattern (candidate pattern)
    (or (car (flx-score candidate pattern helm-flx-cache)) 0))

(defun helm-fuzzy-default-highlight-match (candidate)
    (let* ((pair (and (consp candidate) candidate))
           (display (if pair (car pair) candidate))
            (real (cdr pair)))
      (with-temp-buffer
        (insert display)
        (goto-char (point-min))
        (if (string-match-p " " helm-pattern)
            (cl-loop with pattern = (split-string helm-pattern)
            for p in pattern
            do (when (search-forward p nil t)
                 (add-text-properties
                  (match-beginning 0) (match-end 0) '(face helm-match))))
          (cl-loop with pattern = (cdr (flx-score display
                                                  helm-pattern helm-flx-cache))
                   for index in pattern
            do (add-text-properties
                (1+ index) (+ 2 index) '(face helm-match))))
        (setq display (buffer-string)))
      (if real (cons display real) display)))

(provide 'helm-cfg)
;;; helm-cfg.el ends here
