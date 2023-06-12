;; -*- lexical-binding: t -*-

(std::using-packages
 org-super-agenda
 german-holidays)

(std::autoload org-agenda
  #'std::org::agenda
  #'std::org::agenda::roam-files-with-tags
  #'std::org::agenda::forced-select
  #'std::org::agenda::goto-today
  #'std::org::agenda::toggle-agenda-tag
  #'std::org::agenda::switch-to
  #'std::org::agenda::unschedule
  #'std::org::agenda::mark-habits
  #'std::org::agenda::quit
  #'std::org::agenda::compare-by-todo-state
  #'std::org::agenda::schedule-now
  #'std::org::agenda::open-link-at-line
  #'std::org::agenda::status-mark
  #'std::org::agenda::show-time-left-tf
  #'std::org::agenda::extend-urgent
  #'std::org::agenda::format-date)

(std::with-desktop
 :check (eq major-mode 'org-agenda-mode)
 :cmd #'org-agenda
 :quit #'org-agenda-quit)

(std::after org-agenda

  (std::silent (org-super-agenda-mode))

  (require 'german-holidays)
  (require 'treemacs)

  (evil-set-initial-state 'org-agenda-mode 'motion)

  (add-hook 'org-agenda-mode-hook #'writeroom-mode)

  (std::add-advice #'std::org::agenda::mark-habits :before #'org-agenda-finalize)

  (setf
   org-super-agenda-header-map                      nil
   calendar-holidays
   (append
    holiday-german-BW-holidays
    '((holiday-float 5 0 2 "Muttertag")
      (holiday-fixed 4 17 "Geburtstag V.")
      (holiday-fixed 9 28 "Geburtstag A.") ))
   org-agenda-block-separator
   (concat (propertize (make-string (round (* 0.75 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
   org-super-agenda-header-separator
   (concat (propertize (make-string (round (* 0.75 (frame-width))) ?⎯) 'face 'font-lock-function-name-face) "\n")
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
   org-agenda-format-date                           #'std::org::agenda::format-date
   org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " %i %-15:c")
     (tags   . " %i %-15:c")
     (search . " %i %-15:c"))
   org-agenda-sorting-strategy
   '((agenda user-defined-down priority-down)
     (tags user-defined-down priority-down)
     (search category-keep))
   org-agenda-custom-commands
   `(("a" "2 Wochen"
      ((agenda ""
               ((org-agenda-files
                 (std::if-work-laptop
                  (std::org::agenda::roam-files-with-tags :in '("agenda" "nt"))
                  (std::org::agenda::roam-files-with-tags :in '("agenda"))))
                (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep))
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'nottodo '("TASK" "APPT" "INBOX")))))))
     ("y" "2 Wochen Ungefiltert"
      ((agenda ""
               ((org-agenda-files
                 (std::if-work-laptop
                  (std::org::agenda::roam-files-with-tags :in '("agenda" "nt"))
                  (std::org::agenda::roam-files-with-tags :in '("agenda"))))
                (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep))))))
     ("s" "Inbox"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'mail) "Inbox"))
              (org-agenda-files (list std::org::inbox-file std::org::inbox-nt-file))
              (org-agenda-prefix-format '((todo . "  ")))
              (org-super-agenda-groups
               '((:name "Privat" :tag "privat")
                 (:name "Arbeit" :tag "nt")))))))
     ("d" "Kanban"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'briefcase) "Heute"))
              (org-agenda-files (std::org::agenda::roam-files-with-tags :in '("agenda" "privat")))
              (org-super-agenda-keep-order t)
              (org-agenda-sorting-strategy '(category-up todo-state-up priority-down user-defined-up))
              (org-super-agenda-groups
               `((:discard (:tag "ARCHIVE"
                            :and (:todo "APPT" :timestamp past)))
                 (:name "Dringend"
                  :deadline (before "+1")
                  :face (:append t :background "#FF1A1A" :foreground "#FFFFFF" :weight bold :extend t)
                  :transformer #'std::org::agenda::extend-urgent)
                 (:name "Wichtig"
                  :and (:scheduled (before "+1")
                        :priority "A")
                  :and (:todo "APPT"
                        :timestamp (before "+3")
                        :timestamp (after  "-1")))
                 (:name "Dauerläufer"
                  :and (:todo "LOOP"
                        :scheduled (before "+1")))
                 (:name "Aktiv"
                        :and (:scheduled (before "+1")
                              :not (:priority "D")
                              :not (:tag "email")))
                 (:name "Emails"
                  :tag "email")
                 (:name "Nebenher"
                  :and (:scheduled (before "+1")
                        :priority "D"))
                 (:name "Bald"
                  :and (:todo "APPT"
                        :timestamp future
                        :timestamp (before "+20"))
                  :and (:scheduled future
                        :scheduled (before "+10"))
                  :transformer #'std::org::agenda::show-time-left-tf)
                 (:name "Bereit"
                  :tag "next")
                 (:name "Warteschlange"
                  :tag "wait")
                 (:name "Vielleicht"
                  :tag "maybe")
                 (:discard (:anything))))))))
     ("f" "Kategorien"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'calendar) "Termine"))
              (org-agenda-files (list std::org::appointments-file))
              (org-agenda-sorting-strategy '(ts-up))
              (org-agenda-prefix-format '((todo . "  ")))
              (org-super-agenda-groups
               '((:name none
                  :timestamp future
                  :timestamp today
                  :transformer #'std::org::agenda::show-time-left-tf)
                 (:discard (:anything))))))
       (tags-todo "hh"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'house) "Haushalt"))
                   (org-agenda-files (list std::org::tasks-file))
                   (org-agenda-prefix-format '((tags . "%l%(std::org::agenda::status-mark)")))
                   (org-super-agenda-keep-order t)
                   (org-super-agenda-auto-category-header-format "%s")
                   (org-super-agenda-groups
                    '((:auto-category)))))
       (todo "LOOP"
             ((org-agenda-files (list std::org::tasks-file))
              (org-agenda-overriding-header (concat (treemacs-get-icon-value 'repeat) "Dauerläufer"))
              (org-agenda-prefix-format '((todo . "  ")))
              (org-agenda-sorting-strategy '(scheduled-up))
              (org-super-agenda-groups
               '((:name none :todo "LOOP" :transformer #'std::org::agenda::show-time-left-tf)))))
       (tags-todo "dotts"
                  ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'screen) "Dotts"))
                   (org-agenda-files (list std::org::tasks-file))
                   (org-agenda-prefix-format '((tags . "%l%(std::org::agenda::status-mark)")))
                   (org-super-agenda-auto-category-header-format "Projekt: %s")
                   (org-super-agenda-keep-order t)
                   (org-super-agenda-groups '((:auto-category)))))
       (todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'bookmark) "Lesezeichen"))
              (org-agenda-files (list std::org::bookmarks-file))
              (org-agenda-prefix-format '((todo . "%l%(std::org::agenda::status-mark)")))
              (org-super-agenda-groups
               '((:name "Bücher"  :tag "book")
                 (:name "Artikel" :tag "art")
                 (:name "Videos"  :tag "vid")))))))
     ("j" "Kunde"
       (
        ,@(--map
           `(todo "" ((org-agenda-overriding-header
                       (->>
                        ,it
                        (f-no-ext)
                        (f-filename)
                        (s-split "_")
                        (-map #'s-capitalize)
                        (s-join " ")
                        (concat (treemacs-get-icon-value 'root-closed))))
                      (org-agenda-files (list ,it))
                      (org-agenda-prefix-format '((tags . "%l%(std::org::agenda::status-mark)")))
                      (org-super-agenda-auto-category-header-format "Story: %s")
                      (org-super-agenda-groups
                       '((:discard (:and (:todo "APPT" :timestamp past)))
                         (:name "Termine"
                          :timestamp future
                          :timestamp today
                          :transformer #'std::org::agenda::show-time-left-tf)
                         (:name "Daily" :tag "daily")
                         (:name "Retro" :tag "retro")
                         (:name "Offene Fragen"  :todo "QUST")
                         (:name "Dauerläufer"
                          :and (:todo "LOOP" :scheduled (before "+1")))
                         (:name "Aufgaben" :auto-category)))))
           (std::org::agenda::roam-files-with-tags :in '("kunde" "agenda")))))
     ("k" "NT & Gilde"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'calendar) "Termine"))
              (org-agenda-files (list std::org::appointments-nt-file))
              (org-agenda-sorting-strategy '(ts-up))
              (org-agenda-prefix-format '((todo . "  ")))
              (org-super-agenda-groups
               '((:name none
                  :timestamp future
                  :timestamp today
                  :transformer #'std::org::agenda::show-time-left-tf)
                 (:discard (:anything))))))
       (todo "LOOP"
             ((org-agenda-files (list std::org::tasks-nt-file))
              (org-agenda-overriding-header (concat (treemacs-get-icon-value 'repeat) "Dauerläufer"))
              (org-agenda-prefix-format '((todo . "  ")))
              (org-agenda-sorting-strategy '(scheduled-up))
              (org-super-agenda-groups
               '((:name none :todo "LOOP" :transformer #'std::org::agenda::show-time-left-tf)))))
       (todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'briefcase) "Aufgaben & Projekte"))
              (org-agenda-files (cons std::org::tasks-nt-file
                                      (std::org::agenda::roam-files-with-tags :in '("nt" "agenda" "projekt"))))
              (org-agenda-prefix-format '((todo . "%l%(std::org::agenda::status-mark)")))
              (org-super-agenda-auto-category-header-format "%s:")
              (org-super-agenda-groups
               '((:discard (:todo "LOOP" :todo "APPT"))
                 (:name "Einzelnes"
                  :and (:tag "einzel" :not (:tag "freitag")))
                 (:name "Freitagsmaterial"
                  :and (:tag "einzel" :tag "freitag"))
                 (:name "Offene Fragen"
                  :todo "QUST")
                 (:name "Projekte" :auto-category t)))))
       (todo "BKMR"
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'bookmark) "Lesezeichen"))
              (org-agenda-files (list std::org::bookmarks-nt-file))
              (org-agenda-prefix-format '((todo . "%l%(std::org::agenda::status-mark)")))
              (org-super-agenda-groups
               '((:name "Bücher" :tag "book")
                 (:name "Artikel" :tag "art")
                 (:name "Videos" :tag "vid")))))))
     ("l" "Kanban"
      ((todo ""
             ((org-agenda-overriding-header (concat (treemacs-get-icon-value 'briefcase) "Heute"))
              (org-agenda-files (std::org::agenda::roam-files-with-tags :in '("agenda" "nt")))
              (org-super-agenda-keep-order t)
              (org-agenda-sorting-strategy '(category-up todo-state-up priority-down user-defined-up))
              (org-super-agenda-groups
               `((:discard
                  (:tag "ARCHIVE"
                   :and (:todo "APPT" :timestamp past)))
                 (:name "Dringend"
                        :deadline (before "+1")
                        :face (:append t :background "#FF1A1A" :foreground "#FFFFFF" :weight bold :extend t)
                        :transformer #'std::org::agenda::extend-urgent)
                 (:name "Wichtig"
                        :transformer #'std::org::agenda::show-time-left-tf
                        :and (:scheduled (before "+1") :priority>= "B")
                        :and (:todo "APPT"
                              :timestamp (before "+3d")
                              :timestamp (after  "-1d")))
                 (:name "Dauerläufer"
                        :and (:todo "LOOP" :scheduled today))
                 (:name "Aktiv"
                        :scheduled (before "+1"))
                 (:name "Bald"
                        :and (:todo "APPT" :timestamp future :timestamp (before "+20"))
                        :and (:scheduled future :scheduled (before "+10"))
                        :transformer #'std::org::agenda::show-time-left-tf)
                 (:name "Bereit"
                        :tag "next")
                 (:name "Warteschlange"
                        :tag "wait")
                 (:name "Vielleicht"
                        :tag "maybe")
                 (:name "Unsortiert"
                        :anything))))))) )))

(std::keybind
 :leader
 "aa"    #'std::org::agenda
 "a C-a" #'std::org::agenda::forced-select
 :global
 "<f12>"   #'std::org::agenda
 "C-<f12>" #'std::org::agenda::forced-select)

(std::after org-agenda
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
   "tt"  #'std::org::agenda::toggle-agenda-tag
   "P"   #'org-agenda-priority))
