;; -*- lexical-binding: t -*-

(std::using-packages
 company
 company-posframe
 company-quickhelp
 company-prescient)

(std::autoload completion
  #'std::completion::prose-complete
  #'std::completion::complete-and-keep-frontend
  #'std::completion::prose-hook
  #'std::completion::margin-function)

(std::schedule 1 :no-repeat
  (global-company-mode))

(std::keybind
 :global
 "C-SPC" #'company-complete
 "C-@"   #'company-complete)

(add-hook 'text-mode-hook     #'std::completion::prose-hook)
(add-hook 'markdown-mode-hook #'std::completion::prose-hook)

;; Settings
(std::after company

  (company-prescient-mode)
  (company-posframe-mode)

  (std::add-transient-advice std::completion::enable-quickhelp :before #'company-complete
    (require 'company-quickhelp))

  (setq-default
   company-backends
   '((company-capf company-files :with company-yasnippet)
     (company-dabbrev company-dabbrev-code company-keywords)))

  (setf
   company-abort-manual-when-too-short t
   company-auto-complete               nil
   company-async-timeout               5
   company-dabbrev-code-ignore-case    nil
   company-dabbrev-downcase            nil
   company-dabbrev-ignore-case         nil
   company-idle-delay                  1
   company-minimum-prefix-length       3
   company-require-match               nil
   company-selection-wrap-around       t
   company-show-numbers                t
   company-echo-delay                  nil
   company-format-margin-function      #'std::completion::margin-function
   ;; Tooltip
   company-tooltip-flip-when-above     nil
   company-tooltip-minimum-width       70
   company-tooltip-align-annotations   t
   company-tooltip-margin              2
   ;; posframe
   company-posframe-show-indicator     nil
   company-posframe-show-params
   (list :internal-border-width 2
         :internal-border-color "#1C1C1C"
         :child-frame-border-color "#1C1C1C")))

;; Keybinds
(std::after company
  (std::keybind
   :keymap company-active-map
   "C-SPC" #'std::completion::complete-and-keep-frontend
   "C-j"   #'company-select-next
   "C-k"   #'company-select-previous))

;; Keymap corrections for quickhelp
(std::after company-quickhelp

  (defun std::company::off (_)
    "Use default keys when company is not active. ARG is ignored."
    (std::keybind
     :keymap (evil-normal-state-map evil-insert-state-map)
     "C-j" #'newline-and-indent
     "C-k" #'kill-line)
    (std::keybind
     :keymap evil-insert-state-map
     "C-l" #'yas-expand))

  (defun std::company::on (_)
    "Use company's keys when company is active.
  Necessary due to company-quickhelp using global key maps.
  ARG is ignored."
    (std::keybind
     :keymap (evil-normal-state-map evil-insert-state-map)
     "C-j" #'company-select-next
     "C-k" #'company-select-previous)
    (std::keybind
     :keymap evil-insert-state-map
     "C-l" #'company-quickhelp-manual-begin))

  (add-hook 'company-completion-started-hook   #'std::company::on)
  (add-hook 'company-completion-finished-hook  #'std::company::off)
  (add-hook 'company-completion-cancelled-hook #'std::company::off)

  (define-key company-active-map (kbd "C-l") #'company-quickhelp-manual-begin))

;; Box
(std::after company-box
  (setf company-box-scrollbar nil))
