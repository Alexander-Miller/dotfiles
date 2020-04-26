;; -*- lexical-binding: t -*-

(std::using-packages
 swiper
 helm-ag)

(std::if-version 26
  (std::using-packages
   ivy-posframe))

(std::autoload search
  #'std::search::rg-in-project)

(setf avy-all-windows      t
      avy-case-fold-search nil
      ivy-height           4
      isearch-forward      t)

(std::keybind
  :global
  "C-s"   #'swiper
  "C-M-S" #'swiper-thing-at-point
  "M-o"   #'evil-avy-goto-char-2
  :leader
  "/"  #'std::search::rg-in-project
  "sf" #'helm-do-ag-this-file
  "sd" #'helm-do-ag
  "sc" #'evil-ex-nohighlight)

;; Swiper Settings
(std::after swiper

  (add-to-list 'swiper-font-lock-exclude 'org-mode)

  (std::if-version 26
    (setf ivy-posframe-display-functions-alist
          '((swiper          . ivy-posframe-display-at-frame-bottom-window-center)
            (complete-symbol . ivy-posframe-display-at-point)
            (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
            (t               . ivy-posframe-display)))
    (ivy-posframe-mode)))

;; Swiper Keybinds
(std::after swiper
  (std::keybind :keymap swiper-map
    "C-j" #'ivy-next-line
    "C-k" #'ivy-previous-line
    [escape] #'minibuffer-keyboard-quit))

;; Helm Ag Settings
(std::after helm-ag
  (setf helm-ag-base-command "rg --hidden --vimgrep --no-heading --smart-case"))
