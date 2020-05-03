;; -*- lexical-binding: t -*-

(std::using-packages
 eyebrowse
 winum
 shackle
 window-purpose
 (framey :type git :host github :repo "Alexander-Miller/framey"))

(std::autoload window-management
  #'std::kill-this-buffer
  #'std::pop-to-messages-buffer)

(add-to-list 'window-persistent-parameters '(quit-restore . writable))

(setf
 winum-scope           'frame-local
 helm-display-function #'pop-to-buffer
 shackle-rules
 '(("*helm-ag*"              :select t   :align right :size 0.5)
   ("*helm semantic/imenu*"  :select t   :align right :size 0.4)
   ("*helm org inbuffer*"    :select t   :align right :size 0.4)
   (magit-popup-mode         :select t   :align right :size 0.4)
   (magit-diff-mode          :select nil :align right :size 0.5)
   (magit-log-select-mode    :select nil :align right :size 0.5)
   (flycheck-error-list-mode :select nil :align below :size 0.25)
   (compilation-mode         :select nil :align below :size 0.25)
   (messages-buffer-mode     :select t   :align below :size 0.25)
   (inferior-emacs-lisp-mode :select t   :align below :size 0.25)
   (ert-results-mode         :select t   :align below :size 0.5)
   (calendar-mode            :select t   :align below :size 0.25)
   (racer-help-mode          :select t   :align right :size 0.5)
   (help-mode                :select t   :align right :size 0.5)
   (helpful-mode             :select t   :align right :size 0.5)
   (" *Deletions*"           :select t   :align below :size 0.25)
   (" *Marked Files*"        :select t   :align below :size 0.25)
   ("*Org Select*"           :select t   :align below :size 0.33)
   ("*Org Note*"             :select t   :align below :size 0.33)
   ("*Org Links*"            :select t   :align below :size 0.2)
   (" *Org todo*"            :select t   :align below :size 0.2)
   ("*Man.*"                 :select t   :align below :size 0.5  :regexp t)
   ("*helm.*"                :select t   :align below :size 0.33 :regexp t)
   ("*Org Src.*"             :select t   :align right :size 0.5  :regexp t)))

(defun std::maybe-display-shackle (buffer alist)
  (and (shackle-display-buffer-condition buffer alist)
       (shackle-display-buffer-action buffer alist)))

(setf purpose-action-sequences
      '((switch-to-buffer
         . (purpose-display-reuse-window-buffer
            purpose-display-reuse-window-purpose
            std::maybe-display-shackle
            purpose-display-maybe-same-window
            purpose-display-maybe-other-window
            purpose-display-maybe-other-frame
            purpose-display-maybe-pop-up-window
            purpose-display-maybe-pop-up-frame))

        (prefer-same-window
         . (purpose-display-maybe-same-window
            std::maybe-display-shackle
            purpose-display-reuse-window-buffer
            purpose-display-reuse-window-purpose
            purpose-display-maybe-other-window
            purpose-display-maybe-other-frame
            purpose-display-maybe-pop-up-window
            purpose-display-maybe-pop-up-frame))

        (force-same-window
         . (purpose-display-maybe-same-window
            std::maybe-display-shackle))

        (prefer-other-window

         . (purpose-display-reuse-window-buffer
            purpose-display-reuse-window-purpose
            std::maybe-display-shackle
            purpose-display-maybe-other-window
            purpose-display-maybe-pop-up-window
            purpose-display-maybe-other-frame
            purpose-display-maybe-pop-up-frame
            purpose-display-maybe-same-window))

        (prefer-other-frame
         . (purpose-display-reuse-window-buffer-other-frame
            purpose-display-reuse-window-purpose-other-frame
            std::maybe-display-shackle
            purpose-display-maybe-other-frame
            purpose-display-maybe-pop-up-frame
            purpose-display-maybe-other-window
            purpose-display-maybe-pop-up-window
            purpose-display-reuse-window-buffer
            purpose-display-reuse-window-purpose
            purpose-display-maybe-same-window))))

(eyebrowse-mode)
(winum-mode)
(shackle-mode)
(purpose-mode)
(framey-mode)

(setf purpose-user-mode-purposes
      '((flycheck-error-list-mode . bottom)
        (messages-buffer-mode     . bottom)
        (compilation-mode         . bottom)
        (calendar-mode            . bottom)
        (inferior-emacs-lisp-mode . bottom)))

(purpose-compile-user-configuration)

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
 "w=" #'balance-windows
 "wJ" #'evil-window-move-very-bottom
 "wK" #'evil-window-move-very-top
 "wH" #'evil-window-move-far-left
 "wL" #'evil-window-move-far-right
 "qf" #'delete-frame
 ;; Buffers
 :leader
 "bm"    #'std::pop-to-messages-buffer
 "bd"    #'std::kill-this-buffer
 "b C-d" #'kill-buffer-and-window
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

(cl-defmacro std::with-desktop (&key cmd check quit)
  "Create a wrapper to launch a command in its own eyebrowse desktop.

CMD is the function to wrap.
CHECK is a form to tets whether CMD needs to be run or if just switch the desk
top is sufficient.
QUIT is the exit command that will be adviced to also return to the previously
active desktop."
  (-let [slot std::desktop-slot]
    `(unless (get ,cmd 'std::has-desktop)
       (put ,cmd 'std::has-desktop t)
       (cl-incf std::desktop-slot)
       (advice-add
        ,quit :after
        (lambda () (eyebrowse-switch-to-window-config
                    (get ,cmd 'std::return-to-desktop))))
       (advice-add
        ,cmd :around
        (lambda (fn &rest args)
          (put ,cmd 'std::return-to-desktop (eyebrowse--get 'current-slot))
          (eyebrowse-switch-to-window-config ,slot)
          (delete-other-windows)
          ;; a timer is needed because it looks like we are still in the old
          ;; buffer when the switch has happened
          (run-with-timer
           0 nil
           (lambda (check fn args)
             (unless (funcall check)
               (apply fn args)))
           (lambda () ,check) fn args))))))
