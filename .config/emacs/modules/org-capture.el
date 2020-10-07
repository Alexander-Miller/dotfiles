;; -*- lexical-binding: t -*-

(std::using-packages doct)

(std::autoload org-capture
  #'std::org::mks
  #'std::org::capture-select-template
  #'std::org::capture::select-path
  #'std::org::capture::find-olp)

(defconst std::org::current-year (format-time-string "%Y"))

(std::after org-capture

  (require 'treemacs)

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
              :file ,std::org::inbox-file
              :headline "Private Inbox"
              :template ("* INBOX %i%?"))
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
             (,(concat (treemacs-get-icon-value 'screen) (std::face "Dotts" 'font-lock-builtin-face))
              :keys "d"
              :type entry
              :function std::org::capture::dotts
              :template ("* %{todo-state} %? %{tag}")
              :children
              ((,(std::face "Project" 'font-lock-keyword-face)
                :keys "p"
                :todo-state "PROJ"
                :tag ":P:")
               (,(std::face "Task" 'font-lock-string-face)
                :keys "t"
                :todo-state "NEXT"
                :tag "")))
             (,(concat (treemacs-get-icon-value 'bookmark) "Lesezeichen")
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
                         "%(format-time-string (car org-time-stamp-formats) (time-add (current-time) (time-add 0 (* 60 60 24 10))))")))))))


  ;; Based on https://www.reddit.com/r/emacs/comments/fzuv4f/my_prettified_orgcapture/
  (std::add-advice #'std::org::mks :override #'org-mks)
  (std::add-advice #'std::org::capture-select-template :override #'org-capture-select-template))
