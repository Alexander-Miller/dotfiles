;; -*- lexical-binding: t -*-

(std::using-packages
 company
 company-quickhelp
 company-prescient)

(std::autoload completion
  #'std::completion::prose-complete
  #'std::completion::complete-and-keep-frontend
  #'std::completion::prose-hook)

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
   ;; Tooltip
   company-tooltip-flip-when-above     nil
   company-tooltip-minimum-width       70
   company-tooltip-align-annotations   t
   company-tooltip-margin              2))

;; Keybinds
(std::after company
  (std::keybind
   :keymap company-active-map
   "C-SPC" #'std::completion::complete-and-keep-frontend
   "C-j"   #'company-select-next
   "C-k"   #'company-select-previous))

;; Backend sorting
(std::after company
  (defconst std::completion::backend-priorities
    '((company-fish-shell   . 10)
      (company-shell        . 11)
      (company-shell-env    . 12)
      (company-anaconda     . 10)
      (company-capf         . 10)
      (company-yasnippet    . 60)
      (company-keywords     . 70)
      (company-files        . 80)
      (company-dabbrev-code . 90)
      (company-dabbrev      . 100))
    "Alist of backends' priorities.  Smaller number means higher priority.")

  (define-inline std::completion::priority-of-backend (backend)
    "Will retrieve priority of BACKEND.
  Defauts to 999 if BACKEND is nul or has no priority defined."
    (inline-letevals (backend)
      (inline-quote
       (or (cdr (assoc ,backend std::completion::backend-priorities))
           999))))

  (defun std::completion::priority-compare (c1 c2)
    "Compares the priorities of C1 & C2."
    (let* ((b1   (get-text-property 0 'company-backend c1))
           (b2   (get-text-property 0 'company-backend c2))
           (p1   (std::completion::priority-of-backend b1))
           (p2   (std::completion::priority-of-backend b2))
           (diff (- p1 p2)))
      (< diff 0)))

  (defun std::completion::sort-by-backend-priority (candidates)
    "Will sort completion CANDIDATES according to their priorities."
    (sort candidates #'std::completion::priority-compare))

  (defun std::completion::use-completions-priority-sorting ()
    (add-to-list 'company-transformers #'std::completion::sort-by-backend-priority))

  (add-hook 'fish-mode-hook #'std::completion::use-completions-priority-sorting))


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
