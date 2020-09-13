;; -*- lexical-binding: t -*-

(std::using-packages
 org-super-agenda
 german-holidays)

(std::autoload org-agenda
  #'std::org-agenda::goto-today
  #'std::org-agenda::switch-to
  #'std::org::agenda::mark-habits)

(std::with-desktop
 :check (eq major-mode 'org-agenda-mode)
 :cmd #'org-agenda
 :quit #'org-agenda-quit)

(defconst std::org::private-file (expand-file-name "Privat.org" std::org-dir))
(defconst std::org::work-file (expand-file-name "NT.org" std::org-dir))
(defconst std::org::diary-file (expand-file-name "Diary.org" std::org-dir))

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

  (std::add-advice #'std::org::agenda::mark-habits :before #'org-agenda-finalize)

  (setf
   org-super-agenda-header-map                      nil
   calendar-holidays                                holiday-german-BW-holidays
   org-agenda-block-separator                       (concat (propertize (make-string (round (* 0.75 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-super-agenda-header-separator                (concat (propertize (make-string (round (* 0.75 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-agenda-dim-blocked-tasks                     nil
   org-agenda-include-diary                         t
   org-agenda-inhibit-startup                       nil
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
   `(("INBOX"    . (:background "#FFDDCC" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("APPT"     . (:background "#997799" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("HABIT"    . (:background "#53868B" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("PROJ"     . (:background "#5588BB" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("FRAGE"    . (:background "#55A9A9" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("STORY"    . (:background "#5588BB" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("NEXT"     . (:background "#D46168" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("TODO"     . (:background "#9F8B6F" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("TASK"     . (:background "#B87348" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("MAYBE"    . (:background "#BAAF71" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("WAIT"     . (:background "#999999" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("DONE"     . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("OBSOLET"  . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("ENTFÄLLT" . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("GEKLÄRT"  . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000"))))
   org-agenda-custom-commands
   '(("t" "Tagesagenda"
      ((alltodo ""
        ((org-agenda-span 1)
         (org-agenda-overriding-header "Tagesagenda")
         (org-agenda-sorting-strategy '((agenda todo-state-up)))
         (org-super-agenda-groups
          `((:name ,(concat (treemacs-get-icon-value 'info) "Wichtig")
                   :and (:priority>= "A" :not (:scheduled future))
                   :order 1)
            (:name ,(concat (treemacs-get-icon-value 'repeat) "Habits")
                   :and (:habit t :not (:scheduled future))
                   :order 4)
            (:name ,(concat (treemacs-get-icon-value 'error) "Dringend")
                   :and (:deadline t :not (:habit t :deadline future :scheduled future))
                   :order 2)
            (:name ,(concat (treemacs-get-icon-value 'list) "Heute")
                   :and (:scheduled today :not (:habit t))
                   :and (:scheduled past  :not (:habit t))
                   :and (:deadline t :not (:habit t))
                   :order 3)
            (:name ,(concat (treemacs-get-icon-value 'calendar) "Anstehend")
                   :and (:scheduled t :deadline t)
                   :timestamp future
                   :order 5)
            (:name ,(concat (treemacs-get-icon-value 'suitcase) "Als nächtes")
                   :todo "NEXT"
                   :order 6)
            (:name ,(concat (treemacs-get-icon-value 'next) "Eventuell")
                   :todo "TODO"
                   :order 7)
            (:name ,(concat (treemacs-get-icon-value 'wait) "Warteschlange")
                   :todo "WAIT"
                   :order 8)
            (:discard (:anything))))))))
     ("s" "Std Agenda"
      ((todo "INBOX"
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'mail) "Inbox"))
              (org-super-agenda-groups
               '((:name "Privat:" :file-path "Privat.org")
                 (:name "Arbeit:" :file-path "NT.org")
                 (:discard (:anything))))))
       (tags-todo "dotts"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'screen) "Dotts"))
                   (org-agenda-sorting-strategy '((agenda priority-down todo-state-up)
                                                  (todo priority-down todo-state-up)))
                   (org-super-agenda-groups
                    '((:name "Projekte" :and (:tag "dotts" :todo "PROJ"))
                      (:name "Pakete:"  :tag "pkg")
                      (:name "Emacs:"   :and (:tag "emacs" :not (:tag "P")))
                      (:name "Anderes:" :tag "otherdotts")
                      (:name "Projekteinzelteile:" :tag "dotts")
                      (:discard (:anything))))))
       (tags-todo "hh"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'house) "Haushalt"))))
       (tags "appt"
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'calendar) "Termine"))
              (org-super-agenda-groups
               '((:name none :timestamp future)
                 (:discard (:anything))))))
       (tags-todo "bm"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'bookmark) "Lesezeichen"))
                   (org-agenda-sorting-strategy '((agenda todo-state-down)))
                   (org-super-agenda-groups
                    '((:name "Bücher:" :tag "book")
                      (:name "Artikel: & Blogs [Klein]:" :and (:tag "small" :tag "art"))
                      (:name "Artikel: & Blogs [Groß]:" :and (:tag "large" :tag "art"))
                      (:name "Videos:" :tag "vid")
                      (:discard (:anything))))))
       (agenda "" ())))
     ("a" "NT Agenda"
      ((todo "INBOX"
             ((org-agenda-overriding-header "Inbox")
              (org-agenda-files (list std::org::work-file))))
       (todo ""
             ((org-agenda-overriding-header "Heute")
              (org-agenda-files (list std::org::work-file))
              (org-super-agenda-groups
               '((:name "Kunde"
                        :and (:scheduled today :tag "kunde")
                        :and (:scheduled past  :tag "kunde"))
                 (:name "NT"
                        :scheduled today
                        :scheduled past)
                 (:discard (:anything))))))
       (tags-todo "kunde"
             ((org-agenda-overriding-header "Kundenprojekt")
              (org-agenda-files (list std::org::work-file))
              (org-super-agenda-groups
               '((:name "Stories"        :todo "STORY")
                 (:name "Offene Frangen" :todo "FRAGE")
                 (:name "Offene TODOs"   :todo "TODO")
                 (:name "Anderes"        :anything)))))
       (todo ""
             ((org-agenda-overriding-header "NT & AQE & AEP")
              (org-agenda-files (list std::org::work-file))
              (org-super-agenda-groups
               '((:name "Freitagsmaterial" :tag "fri")
                 (:name "Andere Aufgaben"
                        :and (:todo "PROJ" :tag "nt")
                        :and (:todo "TASK" :tag "nt")
                        :and (:todo "TODO" :tag "nt")
                        :and (:todo "NEXT" :tag "nt"))
                 (:name "Warteschleife"
                        :and (:todo "WAIT" :tag "nt"))
                 (:name "Dauerläufer" :and (:todo "HABIT" :not (:scheduled today)))
                 (:discard (:todo "INBOX" :tag "kunde")))))))))))

;; Keybinds
(std::after org
  (std::keybind
   :evil motion org-agenda-mode-map
   "RET" #'std::org-agenda::switch-to
   "gr"  #'org-agenda-redo
   "."   #'std::org-agenda::goto-today
   "t"   #'org-agenda-todo
   "M-j" #'org-agenda-forward-block
   "M-k" #'org-agenda-backward-block))
