;; -*- lexical-binding: t -*-

(std::using-packages doct)

(std::autoload org-capture
  #'std::org::mks
  #'std::org::capture-select-template
  #'std::org::capture::select-path
  #'std::org::capture::find-olp)

(defconst std::org::current-year (format-time-string "%Y"))
(defconst std::org::work-project (or (getenv "_NT_PROJECT") ""))

(std::after org-capture

  (require 'treemacs)

  (defun std::org::capture::dotts ()
    (let* ((olp `("Vorhaben" "Dotts"))
           (sub-entry (std::org::capture::select-path olp)))
      (std::org::capture::find-olp (append olp (list sub-entry)))))

  (defun std::org::capture::bookmark ()
    (std::org::capture::find-olp
     `("Lesezeichen" ,(std::read ": " '("Bücher" "Artikel" "Video")))))

  (defun std::org::capture::log-timestamp ()
    (format-time-string "[%Y-%m-%d %a]"))

  (setf org-capture-templates
        (doct
         `((,(concat (treemacs-get-icon-value 'house) (std::face "Privat" 'font-lock-string-face))
            :keys "p"
            :file ,std::org::private-file
            :empty-lines-after 1
            :children
            ((,(concat (treemacs-get-icon-value 'mail) (std::face "Inbox" 'font-lock-builtin-face))
              :keys "i"
              :file ,std::org::inbox-file
              :headline "Private Inbox"
              :template("* INBOX %i%?"
                        "%(format-time-string (car org-time-stamp-formats) (time-add (current-time) (time-add 0 (* 60 60 24 10))))"))
             (,(concat (treemacs-get-icon-value 'calendar) (std::face "Termin" 'font-lock-string-face))
              :keys "t"
              :file ,std::org::private-file
              :olp ("Termine")
              :template ("* APPT %? %^T"))
             (,(concat (treemacs-get-icon-value 'fallback) (std::face "Tagebuch" 'font-lock-type-face))
              :keys "a"
              :file ,std::org::diary-file
              :datetree t
              :headline "Tagebuch"
              :type plain)
             (,(concat (treemacs-get-icon-value 'list) (std::face "Haushalt Log" 'font-lock-function-name-face))
              :keys "h"
              :olp ("Haushalt")
              :file ,std::org::private-file
              :datetree t
              :type plain
              :template ("~%^{>_|Wohnung|Strom|Versicherungen|Internet|Sonstiges}~"
                         "%?"))
             (,(concat (treemacs-get-icon-value 'house) (std::face "Haushalt Aufgabe" 'font-lock-constant-face))
              :keys "w"
              :olp ("Vorhaben" "Haushalt")
              :file ,std::org::private-file
              :type entry
              :template ("* %^{TODO: |PROJ|TODO|TASK|NEXT} %?"
                         ""))
             (,(concat (treemacs-get-icon-value 'screen) (std::face "Dotts Aufgabe" 'font-lock-variable-name-face))
              :keys "d"
              :type entry
              :function std::org::capture::dotts
              :template ("* %^{TODO: |PROJ|TODO|TASK|NEXT} %?"))
             (,(concat (treemacs-get-icon-value 'screen) (std::face "Dauerläufer" 'font-lock-doc-face))
              :keys "j"
              :type entry
              :olp ("Vorhaben" "Gewohnheiten")
              :template ("* LOOP %?"
                         ":SCHEDULED: %t"
                         ":PROPETIES:"
                         ":STYLE:    habit"
                         ":END:"))
             (,(concat (treemacs-get-icon-value 'bookmark) (std::face "Lesezeichen" 'font-lock-builtin-face))
              :type entry
              :keys "l"
              :function std::org::capture::bookmark
              :template ("* %^{TODO:|MAYBE|NEXT} %?"
                         "%c"))))

           (,(concat (treemacs-get-icon-value 'briefcase) (std::face "Arbeit" 'font-lock-function-name-face))
            :keys "n"
            :file ,std::org::work-file
            :children
            ((,(concat (treemacs-get-icon-value 'mail) (std::face "Inbox" 'font-lock-function-name-face))
              :keys "i"
              :headline "NT Inbox"
              :file ,std::org::inbox-file
              :template ("* INBOX %i%?"
                         "%(format-time-string (car org-time-stamp-formats) (time-add (current-time) (time-add 0 (* 60 60 24 10))))"))
             (,(concat (treemacs-get-icon-value 'calendar) (std::face "Termin" 'font-lock-string-face))
              :keys "t"
              :file ,std::org::work-file
              :olp ("AQE/NT" "Termine")
              :template ("* APPT %? %^T"))
             (,(concat (treemacs-get-icon-value 'bookmark) (std::face "Lesezeichen" 'font-lock-builtin-face))
              :type entry
              :keys "l"
              :file ,std::org::work-file
              :olp ("AQE/NT" "Lesezeichen")
              :template ("* TODO [[%c][%?]]"))
             (,(concat (treemacs-get-icon-value 'screen) (std::face "NT Aufgabe" 'font-lock-variable-name-face))
              :keys "d"
              :type entry
              :file ,std::org::work-file
              :olp ("AQE/NT" "Aufgaben")
              :template ("* %^{TODO:|TODO|TASK|PROJ} %?"
                         ""))
             (,(concat (treemacs-get-icon-value 'house) (std::face "Projekt Aufgabe" 'font-lock-constant-face))
              :keys "w"
              :olp (,std::org::work-project "Aufgaben")
              :file ,std::org::work-file
              :type entry
              :template ("* %^{TODO: |TODO|TASK|NEXT|APPT} %?"
                         ""))
             (,(concat (treemacs-get-icon-value 'list) (std::face "Story" 'font-lock-type-face))
              :keys "s"
              :olp (,std::org::work-project "Stories")
              :file ,std::org::work-file
              :prepend t
              :type entry
              :template ("* STORY [/] %^{Ticket: } %^{Name: }"
                         ":PROPERTIES:"
                         ":CATEGORY:  %\\2"
                         ":END:")))))))

  ;; Based on https://www.reddit.com/r/emacs/comments/fzuv4f/my_prettified_orgcapture/
  (std::add-advice #'std::org::mks :override #'org-mks)
  (std::add-advice #'std::org::capture-select-template :override #'org-capture-select-template))
