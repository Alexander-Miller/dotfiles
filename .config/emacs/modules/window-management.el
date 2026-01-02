;; -*- lexical-binding: t -*-

(std::using-packages
 eyebrowse
 winum
 shackle
 yequake
 (framey :type git :host github :repo "Alexander-Miller/framey"))

(std::autoload window-management
  #'std::windows::hydra/body
  #'std::windows::yequake-org-capture
  #'std::windows::split-window-right
  #'std::windows::split-window-below
  #'std::windows::highlight-on-select
  #'std::windows::size-change/body)

(add-to-list 'window-persistent-parameters '(quit-restore . writable))
(with-current-buffer (get-buffer-create "*Messages*")
  (evil-motion-state))

(setf
 winner-ring-size           25
 winum-auto-setup-mode-line nil
 winum-scope                'frame-local
 shackle-rules
 '(("*xref*"                 :select t   :align right :size 0.4)
   (" *undo-tree*"           :select t   :align right :size 0.3)
   (magit-popup-mode         :select t   :align right :size 0.4)
   (debugger-mode            :select t   :align below :size 0.4)
   (magit-diff-mode          :select nil :align below :size 0.5)
   (magit-log-select-mode    :select nil :align right :size 0.5)
   ("*Ledger Report*"        :select t   :align below :size 0.5)
   ("*org-roam*"             :select nil :align right :size 0.25)
   (flycheck-error-list-mode :select nil :align below :size 0.25)
   (vterm-mode               :select t   :align below :size 0.25)
   (compilation-mode         :select nil :align below :size 0.25)
   (comint-mode              :select nil :align below :size 0.25)
   (messages-buffer-mode     :select t   :align below :size 0.25)
   (inferior-emacs-lisp-mode :select t   :align below :size 0.25)
   (ert-results-mode         :select t   :align below :size 0.5)
   (calendar-mode            :select t   :align below :size 0.25)
   (racer-help-mode          :select t   :align right :size 0.5)
   (help-mode                :select t   :align right :size 0.5)
   (helpful-mode             :select t   :align right :size 0.5)
   (mu4e-view-mode           :select t   :align below :size 0.65)
   ("CAPTURE-journal.org"    :select t   :align below :size 0.25)
   (" *Embark Actions*"      :select nil :align below :size 0.5)
   (" *Deletions*"           :select t   :align below :size 0.25)
   (" *Marked Files*"        :select t   :align below :size 0.25)
   ("*Org Select*"           :select t   :align below :size 0.33)
   ("*Org Note*"             :select t   :align below :size 0.33)
   ("*Org Links*"            :select t   :align below :size 0.2)
   (" *Org todo*"            :select t   :align below :size 0.2)
   ("*Man.*"                 :select t   :align right :size 0.5  :regexp t)
   ("*Org Src.*"             :select t   :align right :size 0.5  :regexp t)
   ("*Go-Translate*"         :select t   :align right :size 0.5)
   (".*"                     :select t   :align right :size 0.5 :regexpt t)))

(defun std::maybe-display-shackle (buffer alist)
  (and (shackle-display-buffer-condition buffer alist)
       (shackle-display-buffer-action buffer alist)))

(eyebrowse-mode)
(winum-mode)
(shackle-mode)
(framey-mode)
(winner-mode)

(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-1)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-2)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-3)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-4)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-5)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-6)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-7)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-8)
(std::add-advice #'std::windows::highlight-on-select :after #'winum-select-window-9)

(std::after yequake
  (setf
   yequake-frames
   '(("FRAMEY Org Capture"
      (buffer-fns . (std::windows::yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (top . 75)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))

(std::keybind
 ;; Eyebrowse
 :leader
 "1" #'eyebrowse-switch-to-window-config-1
 "2" #'eyebrowse-switch-to-window-config-2
 "3" #'eyebrowse-switch-to-window-config-3
 "4" #'eyebrowse-switch-to-window-config-4
 "5" #'eyebrowse-switch-to-window-config-5
 "6" #'eyebrowse-switch-to-window-config-6
 "7" #'eyebrowse-switch-to-window-config-7
 "8" #'eyebrowse-switch-to-window-config-8
 "9" #'eyebrowse-switch-to-window-config-9
 "0" #'eyebrowse-switch-to-window-config-0
 ;; Winum
 :keymap winum-keymap
 "M-1" #'winum-select-window-1
 "M-2" #'winum-select-window-2
 "M-3" #'winum-select-window-3
 "M-4" #'winum-select-window-4
 "M-5" #'winum-select-window-5
 "M-6" #'winum-select-window-6
 "M-7" #'winum-select-window-7
 "M-8" #'winum-select-window-8
 "M-9" #'winum-select-window-9
 ;; Windows
 :leader
 "W"   #'std::windows::hydra/body
 "w="  #'balance-windows
 "wJ"  #'evil-window-move-very-bottom
 "wK"  #'evil-window-move-very-top
 "wH"  #'evil-window-move-far-left
 "wL"  #'evil-window-move-far-right
 "wl"  #'evil-window-right
 "wh"  #'evil-window-left
 "wj"  #'evil-window-down
 "wk"  #'evil-window-up
 "wsl" #'std::windows::split-window-right
 "wsj" #'std::windows::split-window-below
 "wm"  #'delete-other-windows
 "w0"  #'delete-window
 "wM"  #'treemacs-delete-other-windows
 "wu"  #'winner-undo
 "wr"  #'winner-redo
 "ww"  #'ace-window
 "wW"  #'ace-swap-window
 "qf"  #'delete-frame
 "wS"  #'std::windows::size-change/body
 :evil (normal motion) messages-buffer-mode-map
 "q" #'quit-window)

(std::after Man-mode
  (std::keybind
   :keymap Man-mode-map
   "q" #'kill-buffer-and-window))

(std::after helpful
  (std::keybind
   :evil (normal motion) helpful-mode-map
   "q" #'framey-quit-window))

(defvar std::desktop-slot 11)

(cl-defmacro std::defun-with-desktop (&key name command check)
  "Create a wrapper to launch a command in its own eyebrowse desktop.

CMD is the function to wrap.
CHECK is a form to tets whether CMD needs to be run or if just switch the desk
top is sufficient.
QUIT is the exit command that will be adviced to also return to the previously
active desktop."
  (-let [slot std::desktop-slot]
    `(unless (get ,command 'std::has-desktop)
       (put ,command 'std::has-desktop t)
       (cl-incf std::desktop-slot)
       (defun ,name (&optional force-select)
         (interactive "P")
         (eyebrowse-switch-to-window-config ,slot)
         ;; timer to ensure the current-buffer changes
         (run-with-timer
          0 nil
          (lambda ()
            (when (or force-select (not ,check))
              (delete-other-windows)
              (call-interactively ,command))))))))

(setf display-buffer-alist
      '((std::windows::match-buffer std::windows::display-buffer)
        (shackle-display-buffer-condition shackle-display-buffer-action)))

(defun std::windows::match-buffer (buffer-name &rest args)
  (let ((mode (buffer-local-value 'major-mode (get-buffer buffer-name))))
    (or (ht-get std::windows::purpose-map mode)
        (ht-get std::windows::purpose-map (get mode 'derived-mode-parent)))))

(defun std::windows::display-buffer (buffer-name alist &rest args)
  (let* ((pu (std::windows::buffer-purpose buffer-name))
         (w  (--first
              (eq pu (std::windows::buffer-purpose (window-buffer it)))
              (window-list))))
    (unless (eq pu 'IGNORE)
      (if w
          (progn
            (set-window-buffer w buffer-name)
            (select-window (selected-window) :nr))
        (setf w (shackle-display-buffer-action buffer-name alist))
        (set-window-parameter w 'no-other-window t)))
    w))

(defun std::windows::buffer-purpose (buffer-name)
  (let ((purp nil)
        (mode (buffer-local-value 'major-mode (get-buffer buffer-name))))
    (while (and mode (null purp))
      (setf purp (ht-get std::windows::purpose-map mode)
            mode (get mode 'derived-mode-parent)))
    purp))

(defconst std::windows::purpose-map
  (ht
   ('prog-mode         'main)
   ('lisp-data-mode    'main)
   ('conf-mode         'main)
   ('conf-unix-mode    'main)
   ('i3wm-config-mode  'main)
   ('magit-status-mode 'main)
   ('magit-log-mode    'main)
   ('org-mode          'main)
   ('dired-mode        'main)
   ('yaml-mode         'main)
   ('dashboard-mode    'main)
   ('ledger-mode       'main)
   ('css-mode          'main)

   ('helpful-mode        'right)
   ('help-mode           'right)
   ('Man-mode            'right)
   ('magit-revision-mode 'right)

   ('mu4e-main-mode    'mu-main)
   ('mu4e-headers-mode 'mu-main)
   ('mu4e-view-mode    'mu-bottom)

   ('messages-buffer-mode     'bottom)
   ('flycheck-error-list-mode 'bottom)))
