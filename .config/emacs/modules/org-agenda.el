;; -*- lexical-binding: t -*-

(std::using-packages
 org-super-agenda
 german-holidays)

(std::autoload org-agenda
  #'std::org::agenda
  #'std::org::agenda::forced-select
  #'std::org::agenda::goto-today
  #'std::org::agenda::switch-to
  #'std::org::agenda::unschedule
  #'std::org::agenda::mark-habits
  #'std::org::agenda::quit
  #'std::org::agenda::compare-by-todo-state
  #'std::org::agenda::schedule-now
  #'std::org::agenda::now-plus
  #'std::org::agenda::open-link-at-line
  #'std::org::schedule-dot)

(std::with-desktop
 :check (eq major-mode 'org-agenda-mode)
 :cmd #'org-agenda
 :quit #'org-agenda-quit)

(defconst std::org::private-file (expand-file-name "Privat.org" std::org-dir))
(defconst std::org::work-file (expand-file-name "NT.org" std::org-dir))
(defconst std::org::diary-file (expand-file-name "Diary.org" std::org-dir))
(defconst std::org::inbox-file (expand-file-name "Inbox.org" std::org-dir))

;; Settings
(std::after org-agenda

  (std::silent (org-super-agenda-mode))

  (require 'german-holidays)
  (require 'treemacs)

  (setf org-agenda-files (list std::org::work-file std::org::inbox-file))
  (when (string= "am-laptop" (system-name))
    (add-to-list 'org-agenda-files std::org::private-file))

  (evil-set-initial-state 'org-agenda-mode 'motion)

  (add-hook 'org-agenda-mode-hook #'writeroom-mode)

  (std::add-advice #'std::org::agenda::mark-habits :before #'org-agenda-finalize)

  (setf
   org-super-agenda-header-map                      nil
   calendar-holidays                                holiday-german-BW-holidays
   org-agenda-block-separator                       (concat (propertize (make-string (round (* 0.75 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-super-agenda-header-separator                (concat (propertize (make-string (round (* 0.75 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-agenda-cmp-user-defined                      #'std::org::agenda::compare-by-todo-state
   org-agenda-dim-blocked-tasks                     nil
   org-agenda-include-diary                         t
   org-agenda-remove-tags                           t
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
   org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " %i %-15:c")
     (tags   . " %i %-15:c")
     (search . " %i %-15:c"))
   org-todo-keyword-faces
   `(("INBOX"    . (:background "#FFDDCC" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("APPT"     . (:background "#997799" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
     ("LOOP"     . (:background "#53868B" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
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
   org-agenda-sorting-strategy
   '((agenda user-defined-down priority-down)
     (tags user-defined-down priority-down)
     (search category-keep))
   org-agenda-custom-commands
   `(("a" "2 Wochen"
      ((agenda ""
               ((org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep))
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'nottodo '("TASK" "APPT" "INBOX")))))))
     ("s" "Inbox"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'mail) "Inbox"))
              (org-agenda-files (list std::org::inbox-file))
              (org-agenda-prefix-format '((todo . " %i %-7:c")))
              (org-super-agenda-groups
               '((:name "Privat" :tag "privat")
                 (:name "Arbeit" :tag "nt")))))))
     ("d" "Kanban"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'briefcase) "Heute"))
              (org-agenda-files (list std::org::private-file))
              (org-super-agenda-retain-sorting t)
              (org-agenda-sorting-strategy '(priority-down todo-state-up))
              (org-super-agenda-groups
               `((:name "Dringend"
                        :deadline (before ,(std::org::agenda::now-plus 1 days))
                        :face (:append t :background "#661A1A" :weight bold :extend t))
                 (:name "Wichtig"
                        :and (:scheduled (before ,(std::org::agenda::now-plus 1 days)) :priority>= "B")
                        :and (:todo "APPT"
                                    :timestamp (before ,(std::org::agenda::now-plus 2 days))
                                    :timestamp (after  ,(std::org::agenda::now-plus 0 days))))
                 (:name "Aktiv"
                        :scheduled (before ,(std::org::agenda::now-plus 1 days)))
                 (:name "Warteschlange"
                        :todo "WAIT")
                 (:name "Bald"
                        :scheduled (before ,(std::org::agenda::now-plus 10 days))
                        :and (:todo "APPT" :timestamp future))
                 (:name "Bereit"
                        :todo "PROJ"
                        :todo "NEXT"
                        :todo "TODO")
                 (:discard (:anything))))))))
     ("f" "Kategorien"
      ((tags "appt"
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'calendar) "Termine"))
              (org-agenda-sorting-strategy '(ts-up))
              (org-super-agenda-groups
               '((:name none :timestamp future :timestamp today)
                 (:discard (:anything))))))
       (tags-todo "hh"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'house) "Haushalt"))
                   (org-agenda-prefix-format '((tags . "%l%(std::org::schedule-dot)")))
                   (org-super-agenda-groups
                    '((:auto-category)))))
       (todo "LOOP"
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'repeat) "Dauerläufer"))))
       (tags-todo "dotts"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'screen) "Dotts"))
                   (org-agenda-prefix-format '((tags . "%l%(std::org::schedule-dot)")))
                   (org-super-agenda-category-header-format "Projekt: %s")
                   (org-super-agenda-retain-sorting t)
                   (org-super-agenda-groups
                    '((:auto-category)))))
       (tags-todo "bm"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'bookmark) "Lesezeichen"))
                   (org-agenda-files (list std::org::private-file))
                   (org-super-agenda-groups
                    '((:name "Bücher" :tag "book")
                      (:name "Artikel" :tag "art")
                      (:name "Videos" :tag "vid")))))))
     ("j" "Kunde"
      ((tags-todo "kunde"
                  ((org-agenda-overriding-header "Kundenprojekt")
                   (org-agenda-files (list std::org::work-file))
                   (org-agenda-prefix-format '((tags . "   ")))
                   (org-super-agenda-groups
                    '((:name "Wichtig"
                             :deadline past
                             :priority>= "B"
                             :face (:append t :background "#5D2D2D" :extend t))
                      (:name "Daily"          :tag "daily")
                      (:name "Retro"          :tag "@retro")
                      (:name "Aktiv"          :scheduled (before ,(std::org::agenda::now-plus 1 days)))
                      (:name "Anderes"        :not (:tag "story" :todo "APPT"))
                      (:discard (:anything))))))
       (tags-todo "kunde+story"
                  ((org-agenda-overriding-header "Stories")
                   (org-agenda-prefix-format '((tags . "   ")))
                   (org-super-agenda-category-header "Story: ")
                   (org-super-agenda-groups
                    '((:name "Aufgaben" :auto-category)))))))
     ("k" "NT & AQE & AEP"
      ((tags-todo
        "nt"
        ((org-agenda-overriding-header "Kanban")
         (org-super-agenda-groups
          `((:name "Dringend"
                   :deadline (before ,(std::org::agenda::now-plus 1 days))
                   :face (:background "#661A1A" :weight bold  :append t))
            (:name "Wichtig"
                   :deadline past
                   :priority>= "B"
                   :face (:append t :background "#5D2D2D" :extend t))
            (:name "Aktiv"
                   :scheduled (before ,(std::org::agenda::now-plus 1 days)))
            (:name "Termine"
                   :and (:todo "APPT" :timestamp today)
                   :and (:todo "APPT" :timestamp future))
            (:name "Bald (3d)"
                   :and (:scheduled
                         (before ,(std::org::agenda::now-plus 3 days))
                         :scheduled
                         (after ,(std::org::agenda::now-plus 0 days))))
            (:name "Warteschlange"
                   :and (:todo "WAIT" :tag "nt"))
            (:name "Dauerläufer" :and (:todo "LOOP" :not (:scheduled today)))
            (:name "Lesezeichen" :tag "bkm")
            (:name "Anderes" :not (:todo "INBOX" :todo "APPT" :tag "@P"))
            (:discard (:anything))))))
       (tags-todo
        "nt+@P"
        ((org-agenda-overriding-header "Projektaufteilung")
         (org-super-agenda-category-header "Projekt: ")
         (org-agenda-files (list std::org::work-file))
         (org-agenda-prefix-format '((tags . "   ")))
         (org-super-agenda-groups
          `((:name "Projekte" :auto-category t))))))))))

;; Keybinds
(std::keybind
 :leader
 "aa"    #'std::org::agenda
 "a C-a" #'std::org::agenda::forced-select
 :global
 "<f12>"   #'std::org::agenda
 "C-<f12>" #'std::org::agenda::forced-select)

(std::after org
  (std::keybind
   :evil motion org-agenda-mode-map
   "RET" #'std::org::agenda::switch-to
   "gr"  #'org-agenda-redo
   "."   #'std::org::agenda::goto-today
   "t"   #'org-agenda-todo
   "T"   #'org-agenda-todo-yesterday
   "M-j" #'org-agenda-forward-block
   "M-k" #'org-agenda-backward-block
   "q"   #'std::org::agenda::quit
   "Q"   #'org-agenda-quit
   :mode-leader org-agenda-mode
   "l"   #'org-agenda-later
   "h"   #'org-agenda-earlier
   "ss"  #'org-agenda-schedule
   "sn"  #'std::org::agenda::schedule-now
   "sx"  #'std::org::agenda::unschedule
   "C-o" #'std::org::agenda::open-link-at-line
   "P"   #'org-agenda-priority))
