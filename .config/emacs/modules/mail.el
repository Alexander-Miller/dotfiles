;; -*- lexical-binding: t -*-

(std::using-packages
 mu4e-column-faces)

(std::autoload mail
  #'std::mu4e
  #'std::mail::mu4e-sidebar
  #'std::mail::compose-mode-hook
  #'std::mail::view-mode-hook
  #'std::mail::refresh
  #'std::mail::capture-message
  #'std::mail::move-mail-to-in-progress
  #'std::mail::tag/body)

(std::pushnew load-path "/usr/share/emacs/site-lisp/mu4e")

(autoload #'mu4e "mu4e")

(std::keybind :leader "am" #'std::mu4e)

(std::after mu4e

  (add-hook 'mu4e-compose-mode-hook #'std::mail::compose-mode-hook)
  (add-hook 'mu4e-view-mode-hook #'std::mail::view-mode-hook)

  (evil-set-initial-state 'mu4e-main-mode    'motion)
  (evil-set-initial-state 'mu4e-view-mode    'motion)
  (evil-set-initial-state 'mu4e-headers-mode 'motion)

  (require 'mu4e-org)
  (mu4e-column-faces-mode)

  (defun std::mail::custom-column-handler (column message)
    (declare (side-effect-free t))
    (pcase column
      (:account
       (pcase (aref (nth 4 (s-split "/" (mu4e-message-field message :path))) 0)
         (?q '(:foreground "#999999"))
         (?g '(:foreground "#99cc99"))
         (?w '(:foreground "#cc9999"))))
      (:mailbox
       (-let ((dir (mu4e-message-field message :maildir)))
         (cond
          ((string-suffix-p "Inbox" dir)
           '(:foreground "#55CC55" :weight bold))
          ((string-suffix-p "Unbekannt" dir)
           '(:foreground "#6688BB" :weight bold))
          ((or (string-suffix-p "Spam" dir)
               (string-suffix-p "Unerwünscht" dir))
           '(:foreground "#F2777A" :weight bold))
          ((string-suffix-p "drafts" dir)
           '(:foreground "#F0C674" :weight bold))
          ((string-suffix-p "sent" dir)
           '(:foreground "#999999" :weight bold))
          (t
           '(:foreground "#000000" :weight bold)))))))

  (setf mu4e-column-faces-custom-column-handler #'std::mail::custom-column-handler)

  (std::pushnew mu4e-header-info-custom
    (cons
     :account
     '(:name "Email Account"
       :shortname "Acc"
       :help "Email Account"
       :function (lambda (msg)
                   (nth 4 (s-split "/" (mu4e-message-field msg :path)))))))

  (std::pushnew mu4e-header-info-custom
    (cons
     :mailbox
     '(:name "Mailbox"
             :shortname "Mailbox"
             :help "Mailbox"
             :function (lambda (msg)
                         (-last-item
                          (s-split "/" (mu4e-message-field msg :maildir)))))))

  (setf user-mail-address "alexanderm@web.de"
        user-full-name "Alexander Miller")

  (setf
   smtpmail-smtp-server                     "smtp.web.de"
   smtpmail-smtp-service                    465
   smtpmail-stream-type                     'tls
   smtpmail-servers-requiring-authorization ".*"
   message-send-mail-function               #'smtpmail-send-it

   mu4e-attachment-dir                      "~/Downloads"
   mu4e-confirm-quit                        nil
   mu4e-search-include-related              t
   mu4e-completing-read-function            #'completing-read
   mu4e-sent-messages-behavior              'sent
   mu4e-change-filenames-when-moving        t
   mu4e-use-fancy-chars                     nil
   mu4e-get-mail-command                    "mbsync -a"
   mu4e-headers-draft-mark                  '("D" . "D")
   mu4e-headers-flagged-mark                '("F" . "F")
   mu4e-headers-new-mark                    '("N" . "N")
   mu4e-headers-passed-mark                 '("P" . "P")
   mu4e-headers-replied-mark                '("R" . "R")
   mu4e-headers-seen-mark                   '("S" . "S")
   mu4e-headers-unread-mark                 '("U" . "U")
   mu4e-headers-trashed-mark                '("T" . "T")
   mu4e-headers-attach-mark                 '("A" . "A")
   mu4e-headers-encrypted-mark              '("X" . "X")
   mu4e-headers-signed-mark                 '("G" . "G")
   mu4e-headers-list-mark                   '("L" . "L")
   mu4e-headers-personal-mark               '("E" . "E")
   mu4e-headers-calendar-mark               '("C" . "C")
   mu4e-headers-thread-root-prefix          '("* " . "* ")
   mu4e-headers-thread-first-child-prefix   '("┬ " . "┬ ")
   mu4e-headers-thread-child-prefix         '("│ " . "│ ")
   mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
   mu4e-headers-thread-last-child-prefix    '("└ " . "└ ")
   mu4e-headers-thread-blank-prefix         '("  " . "  ")
   mu4e-headers-thread-orphan-prefix        '("• " . "• ")
   mu4e-headers-thread-single-orphan-prefix '("• " . "• ")
   mu4e-headers-thread-duplicate-prefix     '("= " . "= ")
   mu4e-headers-threaded-label              '("T " . "T ")
   mu4e-headers-full-label                  '("F " . "F ")
   mu4e-headers-related-label               '("R " . "R ")
   mu4e-headers-fields
   `((:human-date   . 10)
     (:flags        . 6)
     (:mailbox      . 11)
     (:account      . 6)
     (:from         . 20)
     (:subject      . ,(- (frame-width) 10 6 11 6 20 14))
     (:tags         . 4)))

  (setf mu4e-bookmarks nil)

  (mu4e-bookmark-define
   "not tag:T and not tag:W"
   "Backlog"
   ?b)
  (mu4e-bookmark-define
   "not from:github*"
   "Not GitHub"
   ?n)
  (mu4e-bookmark-define
   "tag:T"
   "In Progress"
   ?a)
  (mu4e-bookmark-define
   "tag:W"
   "Awaiting Response"
   ?w)
  (mu4e-bookmark-define
   "date:24h.."
   "Last 24 hours"
   ?t)
  (mu4e-bookmark-define
   "date:7d..now"
   "Last 7 days"
   ?o))

(std::after mu4e
  (std::keybind
   :keymap mu4e-main-mode-map
   "u" #'mu4e-update-index
   :evil motion mu4e-headers-mode-map
   ";"   #'std::mail::capture-message
   "J"   #'std::edit::evil-forward-five-lines
   "K"   #'std::edit::evil-backward-five-lines
   "RET" #'mu4e-headers-view-message
   "gr"  #'std::mail::refresh
   "t"   #'std::mail::tag/body
   "+"   #'std::mail::add-tag
   "-"   #'std::mail::remove-tag
   "!"   #'mu4e-headers-mark-for-read
   "\""  #'mu4e-headers-mark-for-unread
   "ü"   #'mu4e-headers-mark-for-flag
   "Ü"   #'mu4e-headers-mark-for-unflag
   "d"   #'mu4e-headers-mark-for-trash
   "="   #'mu4e-headers-mark-for-untrash
   "D"   #'mu4e-headers-mark-for-delete
   "e"   #'mu4e-headers-mark-for-refile
   "b"   #'mu4e-search-bookmark
   "M"   #'mu4e-view-mark-thread
   :evil (normal motion) mu4e-main-mode-map
   "j" #'mu4e~headers-jump-to-maildir
   "b" #'mu4e-search-bookmark
   :evil motion mu4e-view-mode-map
   "C-j" #'mu4e-view-headers-next
   "C-k" #'mu4e-view-headers-prev))
