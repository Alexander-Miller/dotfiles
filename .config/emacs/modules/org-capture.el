;; -*- lexical-binding: t -*-

(std::using-packages doct)

(std::autoload org-capture
  #'std::org::mks
  #'std::org::capture-select-template
  #'std::org::capture::select-path
  #'std::org::capture::find-olp
  #'std::org::capture::select-datetree-tag)

(defconst std::org::current-year (format-time-string "%Y"))

(std::after org-capture

  (add-hook 'org-capture-mode-hook #'std::org::file-setup)

  (require 'treemacs)

  (defun std::org::capture::dotts ()
    (let* ((olp `("Dotts"))
           (sub-entry (std::org::capture::select-path olp)))
      (std::org::capture::find-olp (append olp (list sub-entry)))))

  (defun std::org::capture::bookmark ()
    (std::org::capture::find-olp
     (list (std::read ": " '("Bücher" "Artikel" "Video")))))

  (defun std::org::capture::log-timestamp ()
    (format-time-string "[%Y-%m-%d %a]"))

  (setf
   org-capture-templates
   (doct
    `((,(concat (treemacs-get-icon-value 'house) (std::face "Privat" 'font-lock-string-face))
       :keys "p"
       :empty-lines-after 1
       :children
       ((,(concat (treemacs-get-icon-value 'mail) (std::face "Inbox" 'font-lock-builtin-face))
         :keys "i"
         :file ,std::org::inbox-file
         :template("* IDEA %i%?"
                   "%(format-time-string (car org-time-stamp-formats) (time-add (current-time) (time-add 0 (* 60 60 24 10))))"))
        (,(concat (treemacs-get-icon-value 'calendar) (std::face "Termin" 'font-lock-string-face))
         :keys "t"
         :file ,std::org::appointments-file
         :template ("* APPT %?"
                    "%^T"))
        (,(concat (treemacs-get-icon-value 'fallback) (std::face "Tagebuch" 'font-lock-type-face))
         :keys "a"
         :file ,std::org::diary-file
         :datetree t
         :headline "Tagebuch"
         :type plain)
        (,(concat (treemacs-get-icon-value 'list) (std::face "Haushalt Log" 'font-lock-function-name-face))
         :keys "h"
         :file ,std::org::private-log-file
         :before-finalize std::org::capture::select-datetree-tag
         :datetree t
         :type plain
         :template ("%?"))
        (,(concat (treemacs-get-icon-value 'house) (std::face "Haushalt Aufgabe" 'font-lock-constant-face))
         :keys "w"
         :olp ("Haushalt")
         :file ,std::org::tasks-file
         :type entry
         :template ("* %^{TODO: |TODO|TIME|QUST} %?"
                    ""))
        (,(concat (treemacs-get-icon-value 'screen) (std::face "Dotts Aufgabe" 'font-lock-variable-name-face))
         :keys "d"
         :type entry
         :function std::org::capture::dotts
         :file ,std::org::tasks-file
         :template ("* %^{TODO: |TODO|TIME|INFO|PROJ} %?"))
        (,(concat (treemacs-get-icon-value 'repeat) (std::face "Dauerläufer" 'font-lock-doc-face))
         :keys "j"
         :type entry
         :file ,std::org::tasks-file
         :olp ("Gewohnheiten")
         :template ("* LOOP %?"
                    ":SCHEDULED: %t"
                    ":PROPETIES:"
                    ":STYLE:    habit"
                    ":END:"))
        (,(concat (treemacs-get-icon-value 'bookmark) (std::face "Lesezeichen" 'font-lock-builtin-face))
         :type entry
         :file std::org::bookmarks-file
         :keys "l"
         :function std::org::capture::bookmark
         :template ("* BKMR %?"
                    "%c"))
        (,(concat (treemacs-get-icon-value 'mail-plus) (std::face "Email" 'font-lock-builtin-face))
         :type entry
         :file std::org::tasks-file
         :keys "m"
         :olp ("Emails")
         :template ("* %^{TODO: |TODO|TIME|INFO} %a"))))

      (,(concat (treemacs-get-icon-value 'briefcase) (std::face "Arbeit" 'font-lock-function-name-face))
       :keys "n"
       :children
       ((,(concat (treemacs-get-icon-value 'mail) (std::face "Inbox" 'font-lock-function-name-face))
         :keys "i"
         :file ,std::org::inbox-nt-file
         :template ("* IDEA %i%?"
                    "%(format-time-string (car org-time-stamp-formats) (time-add (current-time) (time-add 0 (* 60 60 24 10))))"))
        (,(concat (treemacs-get-icon-value 'calendar) (std::face "Termin" 'font-lock-string-face))
         :keys "t"
         :file ,std::org::appointments-nt-file
         :template ("* APPT %?"
                    "%^T"))
        (,(concat (treemacs-get-icon-value 'bookmark) (std::face "Lesezeichen" 'font-lock-builtin-face))
         :type entry
         :keys "l"
         :file ,std::org::bookmarks-nt-file
         :function std::org::capture::bookmark
         :template ("* BKMR %?"
                    "%c"))
        (,(concat (treemacs-get-icon-value 'screen) (std::face "Aufgabe" 'font-lock-variable-name-face))
         :keys "d"
         :type entry
         :file ,std::org::tasks-nt-file
         :olp ("Einzelnes")
         :template ("* %^{TODO:|TODO|TIME|INFO} %?"
                    ""))
        (,(concat (treemacs-get-icon-value 'repeat) (std::face "Dauerläufer" 'font-lock-doc-face))
         :keys "j"
         :type entry
         :file ,std::org::tasks-nt-file
         :olp ("Dauerläufer")
         :template ("* LOOP %?"
                    ":SCHEDULED: %t"
                    ":PROPETIES:"
                    ":STYLE:    habit"
                    ":END:"))
        (,(concat (treemacs-get-icon-value 'house) (std::face "Projekt Aufgabe" 'font-lock-constant-face))
         :keys "w"
         :olp ("Durchführung" "Aufgaben")
         :file ,std::org::work-project-file
         :type entry
         :template ("* %^{TODO: |TODO|TIME|INFO} %?"
                    ""))
        (,(concat (treemacs-get-icon-value 'list) (std::face "Projekt Story" 'font-lock-type-face))
         :keys "s"
         :olp ("Durchführung" "Stories")
         :file ,std::org::work-project-file
         :prepend t
         :type entry
         :template ("* STRY [/] %^{Ticket: } %^{Name: }"
                    ":PROPERTIES:"
                    ":CATEGORY:  %\\2"
                    ":END:")))))))

  ;; Based on https://www.reddit.com/r/emacs/comments/fzuv4f/my_prettified_orgcapture/
  (std::add-advice #'std::org::mks :override #'org-mks)
  (std::add-advice #'std::org::capture-select-template :override #'org-capture-select-temstd::org::bookmarks-file))
