;; -*- lexical-binding: t -*-

(std::using-packages
 org-super-agenda
 german-holidays)

(std::autoload org-agenda
  #'std::org-agenda::goto-today)

(defconst std::org::private-file (expand-file-name "Privat.org" std::org-dir))
(defconst std::org::work-file (expand-file-name "NT.org" std::org-dir))

;; Settings
(std::after org-agenda

  (std::silent (org-super-agenda-mode))

  (require 'german-holidays)
  (require 'treemacs)

  (add-to-list 'org-agenda-files (concat org-directory "NT.org"))
  (when (string= "am-laptop" (system-name))
    (add-to-list 'org-agenda-files (concat org-directory "Privat.org")))

  (evil-set-initial-state 'org-agenda-mode 'motion)

  (add-hook 'org-agenda-mode-hook #'writeroom-mode)

  (setf
   org-super-agenda-header-map                      nil
   calendar-holidays                                holiday-german-BW-holidays
   org-agenda-block-separator                       (concat (propertize (make-string (round (* 0.57 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-super-agenda-header-separator                (concat (propertize (make-string (round (* 0.57 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-agenda-dim-blocked-tasks                     nil
   org-agenda-include-diary                         t
   org-agenda-inhibit-startup                       t ;; TODO(2020/04/07): check speed
   org-agenda-skip-deadline-prewarning-if-scheduled nil
   org-agenda-skip-scheduled-if-deadline-is-shown   'not-today
   org-agenda-skip-scheduled-delay-if-deadline      nil
   org-agenda-skip-additional-timestamps-same-entry nil
   org-agenda-skip-scheduled-if-done                nil
   org-agenda-span                                  14
   org-agenda-window-setup                          'only-window
   org-deadline-warning-days                        7
   org-extend-today-until                           2
   org-todo-keyword-faces
   `(("INBOX" . (:background "#FFDDCC" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("HABIT" . (:background "#53868B" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("PROJ"  . (:background "#5588BB" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("NEXT"  . (:background "#9F8B6F" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("TASK"  . (:background "#B87348" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("MAYBE" . (:background "#BAAF71" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("DONE"  . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("WAIT"  . (:background "#999999" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000"))))
   org-agenda-custom-commands
   '(("s" "Std Agenda"
      ((todo "" ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'mail) "Inbox"))
                 (org-super-agenda-groups
                  '((:name "Privat:" :and (:todo "INBOX" :file-path "Privat.org"))
                    (:name "Arbeit:" :and (:todo "INBOX" :file-path "NT.org"))
                    (:discard (:anything))))))
       (tags-todo "dotts" ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'screen) "Dotts"))
                           (org-agenda-sorting-strategy nil)
                           (org-super-agenda-groups
                            '((:name "Projekte" :and (:tag "dotts" :todo "PROJ"))
                              (:name "Pakete:"  :tag "pkg")
                              (:name "Emacs:"   :and (:tag "emacs" :not (:tag "P")))
                              (:name "Anderes:" :tag "otherdotts")
                              (:name "Projekteinzelteile:" :tag "dotts")
                              (:discard (:anything))))))
       (tags-todo "haushalt" ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'house) "Haushalt"))
                           (org-super-agenda-groups
                            '((:anything)))))
       (tags-todo "bm" ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'bookmark) "Lesezeichen"))
                        (org-super-agenda-groups
                         '((:name "Bücher:" :tag "book")
                           (:name "Artikel: & Blogs:" :tag "art")
                           (:name "Videos:" :tag "vid")
                           (:discard (:anything))))))

       (agenda "" ()))
      ;; (todo "" ((org-agenda-overriding-header "Work Stuff")
      ;;           (org-agenda-files (list (concat std::org-dir "/NT.org")))
      ;;           (org-super-agenda-groups
      ;;            '((:todo "INBOX" :name "Inbox")
      ;;              (:todo "NEXT" :name "Tasks")
      ;; (("n" "Agenda"
      ;;   ((todo "INBOX"
      ;;          ((org-agenda-overriding-header
      ;;            (concat (propertize "" 'display '(raise 0.15))" Inbox"))
      ;;           (org-agenda-sorting-strategy '(todo-state-up))))

      ;;    (todo "PROJ"
      ;;          ((org-agenda-overriding-header
      ;;            (concat (propertize "" 'display '(raise 0.15))" Projects"))
      ;;           (org-agenda-sorting-strategy '(category-up priority-down))))

      ;;    (todo "NEXT"
      ;;          ((org-agenda-overriding-header
      ;;            (concat (propertize "" 'display '(raise 0.15)) " Next Tasks"))
      ;;           (org-agenda-sorting-strategy '(priority-down category-up))))

      ;;    (todo "TODO"
      ;;          ((org-agenda-overriding-header
      ;;            (concat (propertize "" 'display '(raise 0.15)) " COLLECTBOX (Unscheduled)"))
      ;;           (org-agenda-skip-function
      ;;            '(org-agenda-skip-entry-if 'scheduled 'deadline))))

      ;;    (todo "WAIT"
      ;;          ((org-agenda-overriding-header
      ;;            (concat (propertize "" 'display '(raise 0.15)) " Waiting"))
      ;;           (org-agenda-sorting-strategy '(todo-state-up))))

      ;;    (agenda "" nil))))
      ;;              (:discard (:anything))))))
      ))))


;; Keybinds
(std::after org
  (std::keybind
   :evil motion org-agenda-mode-map
   "RET" #'org-agenda-switch-to
   "gr"  #'org-agenda-redo
   "."   #'std::org-agenda::goto-today
   "t"   #'org-agenda-todo
   "M-j" #'org-agenda-forward-block
   "M-k" #'org-agenda-backward-block))
