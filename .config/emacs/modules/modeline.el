;; -*- lexical-binding: t -*-

(std::using-packages doom-modeline)

(require 'doom-modeline)

(defun std::modeline::make-xpm (face width height)
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image
        (concat
         (format
          "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
          (length (car data)) (length data) color color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat
                          "\""
                          (cl-loop for d in dl
                                   if (= d 0) collect (string-to-char " ")
                                   else collect (string-to-char "."))
                          (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(std::defface std::modeline::selected-separator-face
  (:background "#559955"))

(std::defface std::modeline::separator-inactive-face
  (:background "#25252A"))

(std::defface std::modeline::num-face
  (:foreground "#997799" :bold nil))

(std::defface std::modeline::major-mode-face
  (:foreground "#997799" :bold t))

(std::defface std::modeline::buffer-id-face
  (:foreground "#C98459" :bold t :box "#000000"))

(std::defface std::modeline::flycheck-info-active-face
  (:foreground "#66DD66" :bold t :box "#000000"))

(std::defface std::modeline::flycheck-warning-active-face
  (:foreground "#DDBA1A" :bold t :box "#000000"))

(std::defface std::modeline::flycheck-error-active-face
  (:foreground "#AB3737" :bold t :box "#000000"))

(std::defface std::modeline::window-purpose-active-face
  (:foreground "#F2777A" :bold t :box "#000000"))

(declare-function winum-get-number "winum")
(declare-function eyebrowse--get "eyebrowse")

(defconst std::modeline::selected-window-xpm
  (eval-when-compile (std::modeline::make-xpm 'std::modeline::selected-separator-face 5 25)))

(defconst std::modeline::unselected-window-xpm
  (eval-when-compile (std::modeline::make-xpm 'std::modeline::separator-inactive-face 5 25)))

(define-inline std::num-to-unicode (n)
  (declare (pure t) (side-effect-free error-free))
  (inline-letevals (n)
    (inline-quote
     (pcase ,n
       (1 " ➊") (2 " ➋") (3 " ➌") (4 " ➍")  (5 " ➎") (6 " ➏")
       (7 " ➐") (8 " ➑") (9 " ➒") (10 " ➓") (_ "")))))

(doom-modeline-def-segment std::modeline::window-number
  (--when-let (winum-get-number)
    (propertize (std::num-to-unicode it)
                'face 'std::modeline::num-face)))

(doom-modeline-def-segment std::modeline::desktop-number
  (propertize (std::num-to-unicode (eyebrowse--get 'current-slot))
              'face 'std::modeline::num-face))

(doom-modeline-def-segment std::modeline::buffer-id
  (propertize (concat " " (buffer-name))
              'face 'std::modeline::buffer-id-face))

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
              'face 'std::modeline::major-mode-face))

(doom-modeline-def-segment std::modeline::window-purpose
  (propertize
   (--if-let (std::windows::buffer-purpose (current-buffer))
       (symbol-name it)
     "---" )
   'face 'std::modeline::window-purpose-active-face))

(doom-modeline-def-segment std::modeline::margin
  "   " "   ")

(defconst std::modeline::flycheck-bullet  " • %s")

(defvar flycheck-mode nil)
(doom-modeline-def-segment std::modeline::flycheck
  (when flycheck-mode
    (let* ((count    (flycheck-count-errors flycheck-current-errors))
           (info (alist-get 'info count))
           (warnings (alist-get 'warning count))
           (errors   (alist-get 'error count)))
      (concat (when info
                (propertize
                 (format std::modeline::flycheck-bullet info)
                 'face 'std::modeline::flycheck-info-active-face))
              (when warnings
                (propertize
                 (format std::modeline::flycheck-bullet warnings)
                 'face 'std::modeline::flycheck-warning-active-face))
              (when errors
                (propertize
                 (format std::modeline::flycheck-bullet errors)
                 'face 'std::modeline::flycheck-error-active-face))))))

(doom-modeline-def-modeline 'std
  '(std::modeline::window-bar
    std::modeline::window-number
    std::modeline::desktop-number
    std::modeline::buffer-id
    std::modeline::major-mode
    std::modeline::flycheck)
  '(std::modeline::window-purpose
    std::modeline::margin))

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

  (doom-modeline-def-segment std::modeline::magit-major-mode
    (propertize (format " %s%s"
                 (format-mode-line mode-name)
                 (if (eq major-mode 'magit-log-mode)
                     (format " [%s]" (string-join magit-buffer-log-args " "))
                   ""))
                'mouse-face 'mode-line-highlight
                'local-map std::modeline::major-mode-local-map
                'face 'std::modeline::major-mode-face))

  (doom-modeline-def-modeline 'magit
    '(std::modeline::window-bar
      std::modeline::window-number
      std::modeline::desktop-number
      std::modeline::buffer-id
      std::modeline::magit-major-mode
      std::modeline::buffer-process)
    '(std::modeline::window-purpose
      std::modeline::margin))

  (std::add-hook 'magit-mode-hook (doom-modeline-set-modeline 'magit)))

;; Treemacs
(std::after treemacs

  (doom-modeline-def-segment std::modeline::treemacs-ws
    (format " Treemacs: %s"
            (treemacs-workspace->name (treemacs-current-workspace))))

  (doom-modeline-def-modeline 'treemy
    '(std::modeline::window-bar
      std::modeline::desktop-number
      std::modeline::treemacs-ws))

  (std::add-hook 'treemacs-mode-hook (doom-modeline-set-modeline 'treemy)))

;; Elfeed
(std::after elfeed

  (doom-modeline-def-segment std::modeline::feeds
    (concat " " (elfeed-search--count-unread)))

  (doom-modeline-def-modeline 'elfeed
    '(std::modeline::window-bar
      std::modeline::desktop-number
      std::modeline::major-mode
      std::modeline::feeds)
    '(std::modeline::window-purpose
      std::modeline::margin))

  (std::add-hook 'elfeed-search-mode-hook (doom-modeline-set-modeline 'elfeed)))

;; Mu4e
(std::after mu4e

  (std::defface std::modeline::search-query-active
    (:foreground "#C98459" :bold t :box "#000000"))

  (std::defface std::modeline::search-query-inactive
    (:foreground "#C98459" :bold t :box "#000000"))

  (std::defface std::modeline::search-query-bracket-active
    (:foreground "#559955" :bold t :box "#000000"))

  (std::defface std::modeline::search-query-bracket-inactive
    (:foreground "#559955" :bold t :box "#000000"))

  (doom-modeline-def-segment std::modeline::mail-search
    (if (doom-modeline--active)
        (concat
         (propertize " [" 'face 'std::modeline::search-query-bracket-active)
         (propertize (propertize (mu4e-last-query) 'face 'std::modeline::search-query-active))
         (propertize "]" 'face 'std::modeline::search-query-bracket-active) )
      (concat
       (propertize " [" 'face 'std::modeline::search-query-bracket-inactive)
       (propertize (propertize (mu4e-last-query) 'face 'std::modeline::search-query-inactive))
       (propertize "]" 'face 'std::modeline::search-query-bracket-inactive))))

  (doom-modeline-def-modeline 'mail
    '(std::modeline::window-bar
      std::modeline::major-mode
      std::modeline::mail-search)
    '(std::modeline::window-purpose
      std::modeline::margin))

  (std::add-hook 'mu4e-headers-mode-hook (doom-modeline-set-modeline 'mail)))

;; Dired
(std::after dired

  (doom-modeline-def-segment std::modeline::dired-major-mode
    (propertize " Dired" 'face 'std::modeline::major-mode-face))

  (doom-modeline-def-modeline 'dired
    '(std::modeline::window-bar
      std::modeline::window-number
      std::modeline::desktop-number
      std::modeline::buffer-id
      std::modeline::dired-major-mode))

  (std::add-hook 'dired-mode-hook (doom-modeline-set-modeline 'dired)))
