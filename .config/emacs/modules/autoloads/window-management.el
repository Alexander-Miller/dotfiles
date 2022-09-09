;; -*- lexical-binding: t -*-

(defun std::windows::yequake-org-capture ()
  "Yequake wrapper for fullscreen capture."
  (-let [shackle-rules
         (std::cons
          '("*Org Select*" :select t :same t :size 1.0)
          '("CAPTURE.*"    :select t :same t :size 1.0 :regexp t)
          shackle-rules)]
    (yequake-org-capture)))

(defun std::windows::split-window-right ()
  "Open a new window on the right."
  (interactive)
  (select-window (split-window-right)))

(defun std::windows::split-window-below ()
  "Open a new window on the below."
  (interactive)
  (select-window (split-window-below)))

(defhydra std::windows::size-change (:exit nil :hint t)
  ("j" (enlarge-window 5)    "Enlarge Vertically")
  ("k" (enlarge-window -5)   "Shrink Vertically")
  ("l" (enlarge-window 5  t) "Enlarge Horizontally")
  ("h" (enlarge-window -5 t) "Shrink Horizontally")
  ("q" nil "cancel"))

(defhydra std::windows::hydra (:exit t :hint t)
  ("="   #'balance-windows                 "Balance Windows")
  ("J"  #'evil-window-move-very-bottom     "Move Window To Bottom")
  ("K"  #'evil-window-move-very-top        "Move Window To Top")
  ("H"  #'evil-window-move-far-left        "Move Window Left")
  ("L"  #'evil-window-move-far-right       "Move Window Right")
  ("l"  #'evil-window-right                "Select Right Window")
  ("h"  #'evil-window-left                 "Select Left Window")
  ("j"  #'evil-window-down                 "Select Window Below")
  ("k"  #'evil-window-up                   "Select Window Above")
  ("sl" #'std::windows::split-window-right "Split Right")
  ("sj" #'std::windows::split-window-below "Split Left")
  ("m"  #'delete-other-windows             "Maximize Window")
  ("0"  #'delete-window                    "Delete Window")
  ("M"  #'treemacs-delete-other-windows    "Treemacs-Maximize")
  ("u"  #'winner-undo                      "Winner Undo")
  ("r"  #'winner-redo                      "Winner Redo")
  ("w"  #'ace-window                       "Ace Select")
  ("W"  #'ace-swap-window                  "Ace Swap")
  ("qf"  #'delete-frame                    "Delete Frame")
  ("S"  #'std::windows::size-change/body   "Change Size"))
