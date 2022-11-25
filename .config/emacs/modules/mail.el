;; -*- lexical-binding: t -*-

(std::using-packages
 mu4e-column-faces
 mu4e-alert)

(std::autoload mail
  #'std::mail::compose-mode-hook
  #'std::mail::view-mode-hook
  #'std::mail::refresh
  #'std::mail::tag/body)

(std::pushnew load-path "/usr/share/emacs/site-lisp/mu4e")

(autoload #'mu4e "mu4e")

(std::with-desktop
 :check (memq major-mode '(mu4e-main-mode mu4e-view-mode mu4e-headers-mode mu4e-compose-mode))
 :cmd #'mu4e
 :quit #'mu4e-quit)

(std::keybind :leader "am" #'mu4e)

(std::after mu4e

  (add-hook 'mu4e-compose-mode-hook #'std::mail::compose-mode-hook)
  (add-hook 'mu4e-view-mode-hook #'std::mail::view-mode-hook)

  (evil-set-initial-state 'mu4e-main-mode    'motion)
  (evil-set-initial-state 'mu4e-view-mode    'motion)
  (evil-set-initial-state 'mu4e-headers-mode 'motion)


  (require 'org-mu4e)
  (mu4e-column-faces-mode)

  (mu4e-alert-enable-notifications)
  (mu4e-alert-disable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)

  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

  (setf
   mu4e-alert-email-notification-types '(subjects)
   mu4e-alert-notify-repeated-mails    nil
   mu4e-alert-set-window-urgency       nil
   mu4e-alert-icon                     "email")

  (setf user-mail-address "alexanderm@web.de"
        user-full-name "Alexander Miller")

  (setf
   mu4e-attachment-dir                      "~/Downloads"
   mu4e-confirm-quit                        nil
   mu4e-completing-read-function            #'completing-read
   mu4e-view-use-gnus                       t
   mu4e-sent-messages-behavior              'sent
   mu4e-maildir                             (expand-file-name "~/.mail")
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
   `((:date         . 10)
     (:flags        . 6)
     (:tags         . 4)
     (:mailing-list . 10)
     (:from         . 22)
     (:subject . ,(- (frame-width) 10 6 10 22 8 4))))


  (setf
   mu4e-marks
   '((tag
      :char "t"
      :prompt "gtag"
      :ask-target
      (lambda nil (read-string "What tag do you want to add? "))
      :action
      (lambda (docid msg target) (mu4e-action-retag-message msg target)))

     (refile
      :char ("r" . "r")
      :prompt "refile"
      :dyn-target
      (lambda (target msg) (mu4e-get-refile-folder msg))
      :action
      (lambda (docid msg target)
        (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))

     (delete
      :char ("D" . "D")
      :prompt "Delete"
      :show-target (lambda (target) "delete")
      :action (lambda (docid msg target) (mu4e--server-remove docid)))

     (flag
      :char ("+" . "+")
      :prompt "+flag"
      :show-target (lambda (target) "flag")
      :action
      (lambda (docid msg target)
        (mu4e--server-move docid nil "+F-u-N")))

     (move
      :char ("m" . "m")
      :prompt "move"
      :ask-target mu4e--mark-get-move-target
      :action (lambda (docid msg target)
                (mu4e--server-move docid
                                (mu4e--mark-check-target target)
                                "-N")))

     (read
      :char ("!" . "!")
      :prompt "!read"
      :show-target (lambda (target) "read")
      :action (lambda (docid msg target)
                (mu4e--server-move docid nil "+S-u-N")))

     (trash
      :char ("d" . "d")
      :prompt "dtrash"
      :dyn-target (lambda (target msg)
                    (mu4e-get-trash-folder msg))
      :action (lambda (docid msg target)
                (mu4e--server-move docid
                                (mu4e--mark-check-target target)
                                "+T-N")))

     (unflag
      :char ("-" . "-")
      :prompt "-unflag"
      :show-target (lambda (target) "unflag")
      :action (lambda (docid msg target)
                (mu4e--server-move docid nil "-F-N")))

     (untrash
      :char ("=" . "=")
      :prompt "=untrash"
      :show-target (lambda (target) "untrash")
      :action (lambda (docid msg target)
                (mu4e--server-move docid nil "-T")))

     (unread
      :char "?"
      :prompt "?unread"
      :show-target (lambda (target) "unread")
      :action (lambda (docid msg target)
                (mu4e--server-move docid nil "-S+u-N")))

     (unmark
      :char " "
      :prompt "unmark"
      :action (mu4e-error "No action for unmarking"))

     (action
      :char ("a" . "a")
      :prompt "action"
      :ask-target (lambda ()
                    (mu4e-read-option "Action: " mu4e-headers-actions))
      :action (lambda (docid msg actionfunc)
                (save-excursion
                  (when (mu4e~headers-goto-docid docid)
                    (mu4e-headers-action actionfunc)))))

     (something
      :char ("*" . "*")
      :prompt "*something"
      :action (mu4e-error "No action for deferred mark"))))

  (setf mu4e-bookmarks nil)
  (mu4e-bookmark-define
   "flag:unread AND NOT flag:trashed"
   "Unread Messages"
   ?u)
   (mu4e-bookmark-define
    "date:24h.."
    "Last 24 hours"
     ?t)
   (mu4e-bookmark-define
    "date:7d..now"
    "Last 7 days"
    ?w)
   (mu4e-bookmark-define
    "github"
    "Github Messages"
    ?g)
   (mu4e-bookmark-define
    "mime:image/*"
    "Messages with images"
    ?p))

(std::after mu4e
  (std::keybind
    :keymap mu4e-main-mode-map
    "u" #'mu4e-update-index
    :evil motion mu4e-headers-mode-map
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
    :mode-leader mu4e-headers-mode
    "m"   #'std::mail::mark/body
    :evil (normal motion) mu4e-main-mode-map
    "j" #'mu4e~headers-jump-to-maildir
    "b" #'mu4e-search-bookmark
    :evil motion mu4e-view-mode-map
    "C-j" #'mu4e-view-headers-next
    "C-k" #'mu4e-view-headers-prev))
