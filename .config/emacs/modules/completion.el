;; -*- lexical-binding: t -*-

(std::using-packages
 company
 company-posframe
 company-prescient)

(std::autoload completion
  #'std::completion::quickhelp-poshandler
  #'std::completion::prose-complete
  #'std::completion::complete-and-keep-frontend
  #'std::completion::prose-hook
  #'std::completion::margin-function
  #'std::completion::scroll-quickhelp-up
  #'std::completion::scroll-quickhelp-down)

(std::schedule 1 :no-repeat
  (global-company-mode))

(std::keybind
 :global
 "C-SPC" #'company-complete
 "C-@"   #'company-complete)

(add-hook 'text-mode-hook     #'std::completion::prose-hook)
(add-hook 'markdown-mode-hook #'std::completion::prose-hook)

(std::after company

  (company-prescient-mode)
  (company-posframe-mode)

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
   company-files-chop-trailing-slash   nil
   company-idle-delay                  nil
   company-posframe-quickhelp-delay    5
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
   company-posframe-show-metadata      nil
   company-posframe-show-params
   (list :internal-border-width 2
         :internal-border-color "#1C1C1C")
   ;; posframe quickhelp
   company-posframe-quickhelp-x-offset    3
   company-posframe-quickhelp-show-header nil
   company-posframe-quickhelp-show-params
   (list :poshandler #'std::completion::quickhelp-poshandler
         :internal-border-width 2
         :internal-border-color "#1C1C1C"
         :timeout 60
         :no-properties nil)))

(std::after company
  (std::keybind
   :keymap company-active-map
   "M-j"   #'std::completion::complete-and-keep-frontend
   "C-j"   #'company-select-next
   "C-k"   #'company-select-previous))

;; Keymap corrections for quickhelp
(std::after company-posframe

  (defun std::company::off (_)
    "Use default keys when company is not active. ARG is ignored."
    (std::keybind
     :keymap (evil-normal-state-map evil-insert-state-map)
     "C-j" #'newline-and-indent
     "C-k" #'kill-line
     "C-l" #'recenter-top-bottom)
    (std::keybind
     :keymap evil-insert-state-map
     "C-l" #'yas-expand))

  (defun std::company::on (_)
    "Use company's keys when company is active.
  Necessary due to company-quickhelp using global key maps.
  ARG is ignored."
    (std::keybind
     :keymap (evil-normal-state-map evil-insert-state-map)
     "C-l" #'std::completion::quickhelp-show
     "C-j" #'company-select-next
     "C-k" #'company-select-previous)
    (std::keybind
     :keymap evil-insert-state-map
     "C-l" #'std::completion::quickhelp-show))

  (add-hook 'company-completion-started-hook   #'std::company::on)
  (add-hook 'company-completion-finished-hook  #'std::company::off)
  (add-hook 'company-completion-cancelled-hook #'std::company::off)

  (std::keybind
   :keymap company-posframe-active-map
    "C-M-j" #'std::completion::scroll-quickhelp-down
    "C-M-k" #'std::completion::scroll-quickhelp-up
    "<f1>"  #'std::completion::helpful-for-candidate
    "C-l"   #'std::completion::quickhelp-show))
