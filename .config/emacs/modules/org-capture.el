;; -*- lexical-binding: t -*-

(std::using-packages doct)

(std::autoload org-capture
  #'std::org::mks
  #'std::org::capture-select-template
  #'std::org::capture::select-path
  #'std::org::capture::find-olp)

(defconst std::org::current-year (format-time-string "%Y"))

(std::after org-capture

  (defun std::org::capture::haushalt-log-olp ()
    (std::org::capture::find-olp
     (list "Haushalt" std::org::current-year)))

  (defun std::org::capture::habit-olp ()
    (std::org::capture::find-olp
     (list "Vorhaben" std::org::current-year "Gewohnheiten")))

  (defun std::org::capture::dotts ()
    (let* ((olp `("Vorhaben" ,(format-time-string "%Y") "Dotts"))
           (sub-entry (std::org::capture::select-path olp)))
      (std::org::capture::find-olp (append olp (list sub-entry)))))

  (defun std::org::capture::bookmark ()
    (let* ((olp `("Lesezeichen" ,(format-time-string "%Y")))
           (sub-entry (std::org::capture::select-path olp)))
      (std::org::capture::find-olp (append olp (list sub-entry)))))

  (defun std::org::capture::log-timestamp ()
    (format-time-string "[%Y-%m-%d %a]"))

  (setf org-capture-templates
        (doct
         `((,(concat (treemacs-get-icon-value 'house) (std::face "Privat" 'font-lock-string-face))
            :keys "p"
            :file ,std::org::private-file
            :empty-lines-after 1
            :children
            ((,(concat (treemacs-get-icon-value 'mail) (std::face "Inbox" 'font-lock-string-face))
              :keys "i"
              :headline "Inbox"
              :template ("* INBOX %i%?"))
             (,(concat (treemacs-get-icon-value 'list) (std::face "Haushalt Log" 'font-lock-function-name-face))
              :keys "h"
              :olp ("Haushalt" ,std::org::current-year)
              :file ,std::org::private-file
              :type item
              :empty-lines 1
              :template (" - %^{>_|Wohnung|Strom|Versicherungen|Internet|Sonstiges} :: %(std::org::capture::log-timestamp)"
                         "   %?"))
             (,(concat (treemacs-get-icon-value 'screen) (std::face "Dotts" 'font-lock-builtin-face))
              :keys "d"
              :type entry
              :function std::org::capture::dotts
              :template ("* NEXT %?"))
             (,(concat (treemacs-get-icon-value 'repeat) (std::face "Gewohnheit" 'font-lock-variable-name-face))
              :type entry
              :keys "g"
              :olp ("Vorhaben" ,std::org::current-year "Gewohnheiten")
              :template ("* HABIT %?"
                         "SCHEDULED: %t"
                         ":PROPERTIES:"
                         ":STYLE:    habit"
                         ":END:"
                         ":LOGBOOK:"
                         ":END:"))
             (,(concat (treemacs-get-icon-value 'bookmark) "Lesezeichen")
              :type entry
              :keys "l"
              :function std::org::capture::bookmark
              :template ("* %^{TODO:|MAYBE|NEXT} %?"
                         "%c"))))
           (,(concat (treemacs-get-icon-value 'suitcase) (std::face "Arbeit" 'font-lock-function-name-face))
            :keys "n"
            :file ,std::org::work-file
            :children
            ((,(concat (treemacs-get-icon-value 'mail) (std::face "Inbox" 'font-lock-function-name-face))
              :keys "i"
              :headline "Inbox"
              :template ("* INBOX %i%?"
                         "%(format-time-string (car org-time-stamp-formats) (time-add (current-time) (time-add 0 (* 60 60 24 10))))")))))))


  ;; Based on https://www.reddit.com/r/emacs/comments/fzuv4f/my_prettified_orgcapture/
  (std::advice-add #'std::org::mks :override #'org-mks)
  (std::advice-add #'std::org::capture-select-template :override #'org-capture-select-template))
