;; -*- lexical-binding: t -*-

(std::using-packages
 swiper
 helm-ag)

(std::if-version 26
  (std::using-packages
   (framey :type git :host github :repo "Alexander-Miller/framey")))

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
 "M-O"   #'evil-avy-goto-char
 :leader
 "/"  #'std::search::rg-in-project
 "sf" #'helm-do-ag-this-file
 "sd" #'helm-do-ag
 "sc" #'evil-ex-nohighlight
 ;; jumping
 "jf" #'find-function
 "jl" #'avy-goto-line)

;; Swiper Settings
(std::after swiper

  (add-to-list 'swiper-font-lock-exclude 'org-mode)

  (std::if-version 26
    (require 'framey-swiper)))

;; Swiper Keybinds
(std::after swiper
  (std::keybind
   :keymap swiper-map
   "C-j" #'ivy-next-line
   "C-k" #'ivy-previous-line
   [escape] #'minibuffer-keyboard-quit))

;; Helm Ag Settings
(std::after helm-ag
  (setf helm-ag-base-command "rg --hidden --vimgrep --no-heading --smart-case"))
