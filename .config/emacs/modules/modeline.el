;; -*- lexical-binding: t -*-

(std::using-packages doom-modeline)

(defface std::modeline::selected-separator-face
  '((t (:background "#559955")))
  ""
  :group 'std)

(defface std::modeline::separator-inactive-face
  '((t (:background "#25252a")))
  ""
  :group 'std)

(defface std::modeline::num-face
  '((t (:foreground "#997799" :bold nil)))
  ""
  :group 'std)

(defface std::modeline::num-inactive-face
  '((t (:foreground "#997799" :background "#25252a" :bold nil)))
  ""
  :group 'std)

(defface std::modeline::major-mode-face
  '((t (:foreground "#997799" :bold t)))
  ""
  :group 'std)

(defface std::modeline::major-mode-inactive-face
  '((t (:foreground "#997799" :background "#25252a" :bold t)))
  ""
  :group 'std)

(defface std::modeline::buffer-id-inactive
  '((t (:foreground "#c98459" :background "#25252a" :bold t :box "#000000")))
  ""
  :group 'std)

(require 'doom-modeline)

(declare-function winum-get-number "winum")
(declare-function eyebrowse--get "eyebrowse")

(defconst std::modeline::selected-window-xpm
  (eval-when-compile (doom-modeline--make-xpm 'std::modeline::selected-separator-face 5 30)))

(defconst std::modeline::unselected-window-xpm
  (eval-when-compile (doom-modeline--make-xpm 'std::modeline::separator-inactive-face 5 30)))

(define-inline std::num-to-unicode (n)
  (inline-letevals (n)
    (inline-quote
     (pcase ,n
       (1 " ➊") (2 " ➋") (3 " ➌") (4 " ➍")  (5 " ➎") (6 " ➏")
       (7 " ➐") (8 " ➑") (9 " ➒") (10 " ➓") (_ "")))))

(doom-modeline-def-segment std::modeline::window-number
  (--when-let (winum-get-number)
    (propertize (std::num-to-unicode it)
                'face (if (doom-modeline--active)
                          'std::modeline::num-face
                        'std::modeline::num-inactive-face))))

(doom-modeline-def-segment std::modeline::desktop-number
  (propertize (std::num-to-unicode (eyebrowse--get 'current-slot))
              'face (if (doom-modeline--active)
                        'std::modeline::num-face
                      'std::modeline::num-inactive-face)))

(doom-modeline-def-segment std::modeline::buffer-id
  (propertize (concat " " (buffer-name))
              'face (if (doom-modeline--active)
                        'mode-line-buffer-id
                      'std::modeline::buffer-id-inactive)))

(doom-modeline-def-segment std::modeline::window-bar
  (if (doom-modeline--active)
      std::modeline::selected-window-xpm
    std::modeline::unselected-window-xpm))

(defconst std::modeline::major-mode-local-map
  (eval-when-compile
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1]
        `(menu-item ,(purecopy "Menu Bar") ignore
                    :filter (lambda (_) (mouse-menu-major-mode-map))))
      (define-key map [mode-line mouse-2] 'describe-mode)
      (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
      map)))

(doom-modeline-def-segment std::modeline::major-mode
  (propertize (concat " " (format-mode-line mode-name))
              'mouse-face 'mode-line-highlight
              'local-map std::modeline::major-mode-local-map
              'face (if (doom-modeline--active)
                        'std::modeline::major-mode-face
                      'std::modeline::major-mode-inactive-face)))

(defconst std::modeline::flycheck-bullet-info  (eval-when-compile (propertize " • %s" 'face 'doom-modeline-info)))
(defconst std::modeline::flycheck-bullet-warn  (eval-when-compile (propertize " • %s" 'face 'doom-modeline-warning)))
(defconst std::modeline::flycheck-bullet-error (eval-when-compile (propertize " • %s" 'face 'doom-modeline-urgent)))

(defvar flycheck-mode nil)
(doom-modeline-def-segment std::modeline::flycheck
  (when flycheck-mode
    (let* ((count    (flycheck-count-errors flycheck-current-errors))
           (warnings (alist-get 'warning count))
           (errors   (alist-get 'error count)))
      (concat (when warnings (format std::modeline::flycheck-bullet-warn warnings))
              (when errors   (format std::modeline::flycheck-bullet-error errors))))))

(doom-modeline-def-modeline 'std
  '(std::modeline::window-bar
    std::modeline::window-number
    std::modeline::desktop-number
    std::modeline::buffer-id
    std::modeline::major-mode
    std::modeline::flycheck))

(doom-modeline-set-modeline 'std :global-default)

(std::schedule 0.3 :no-repeat
  (dolist (buffer '("*Messages*" "*scratch*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (setq-local mode-line-format (default-value 'mode-line-format))
        (doom-modeline-set-selected-window)
        (force-mode-line-update)))))

;; Magit
(std::after magit

  (doom-modeline-def-segment std::modeline::buffer-process
    mode-line-process)

  (doom-modeline-def-modeline 'magit
    '(std::modeline::window-bar
      std::modeline::window-number
      std::modeline::desktop-number
      std::modeline::buffer-id
      std::modeline::major-mode
      std::modeline::buffer-process))

  (defun std::modeline::magit-modeline ()
    (doom-modeline-set-modeline 'magit))

  (add-hook 'magit-mode-hook #'std::modeline::magit-modeline))

;; Treemacs
(std::after treemacs

  (doom-modeline-def-modeline 'treemy
    '(std::modeline::window-bar
      std::modeline::desktop-number
      std::modeline::major-mode))

  (defun std::modeline::treemacs ()
    (doom-modeline-set-modeline 'treemy))

  (add-hook 'treemacs-mode-hook #'std::modeline::treemacs))


;; Elfeed
(std::after elfeed

  (doom-modeline-def-segment std::modeline::feeds
    (concat " " (elfeed-search--count-unread)))

  (doom-modeline-def-modeline 'elfeed
    '(std::modeline::window-bar
      std::modeline::desktop-number
      std::modeline::major-mode
      std::modeline::feeds))

  (defun std::modeline::elfeed ()
    (doom-modeline-set-modeline 'elfeed))

  (add-hook 'elfeed-search-mode-hook #'std::modeline::elfeed))

;; Mu4e
(std::after mu4e

  (doom-modeline-def-segment std::modeline::mail-search
    (concat " [" (propertize (mu4e-last-query) 'face 'font-lock-variable-name-face) "]"))

  (doom-modeline-def-modeline 'mail
    '(std::modeline::window-bar
      std::modeline::major-mode
      std::modeline::mail-search))

  (defun std::modeline::mail ()
    (doom-modeline-set-modeline 'mail))

  (add-hook 'mu4e-headers-mode-hook #'std::modeline::mail))
