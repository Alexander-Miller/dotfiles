;; -*- lexical-binding: t -*-

(std::using-packages
 (mu4e :host github :repo "emacsmirror/mu4e" :files (:defaults "mu4e/*.el"))
 mu4e-alert)

(std::autoload mail
  #'std::mail::compose-mode-hook
  #'std::mail::view-mode-hook
  #'std::mail::refresh)

(std::with-desktop
 :check (memq major-mode '(mu4e-main-mode mu4e-headers-mode mu4e-compose-mode))
 :cmd #'mu4e
 :quit #'mu4e-quit)

(std::keybind :leader "am" #'mu4e)

(add-hook 'mu4e-compose-mode-hook #'std::mail::compose-mode-hook)
(add-hook 'mu4e-view-mode-hook #'std::mail::view-mode-hook)

(evil-set-initial-state 'mu4e-main-mode 'motion)
(evil-set-initial-state 'mu4e-view-mode 'motion)
(evil-set-initial-state 'mu4e-headers-mode 'motion)

;; Settings
(std::after mu4e

  (require 'org-mu4e)

  (mu4e-alert-enable-notifications)
  (mu4e-alert-disable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)

  (setf
   mu4e-alert-email-notification-types '(subjects)
   mu4e-alert-notify-repeated-mails    nil
   mu4e-alert-set-window-urgency       nil
   mu4e-alert-icon                     "email")

  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

  (setf user-mail-address "alexanderm@web.de"
        user-full-name "Alexander Miller")

  (setf mu4e-confirm-quit                 nil
        mu4e-sent-messages-behavior       'delete
        mu4e-maildir                      (expand-file-name "~/.mail")
        mu4e-change-filenames-when-moving t
        mu4e-use-fancy-chars              nil
        mu4e-get-mail-command             "mbsync -a"
        mu4e-headers-draft-mark           '("D" . "⚒")
        mu4e-headers-flagged-mark         '("F" . "✚")
        mu4e-headers-new-mark             '("N" . "✱")
        mu4e-headers-passed-mark          '("P" . "❯")
        mu4e-headers-replied-mark         '("R" . "❮")
        mu4e-headers-seen-mark            '("S" . "✔")
        mu4e-headers-trashed-mark         '("T" . "⏚")
        mu4e-headers-attach-mark          '("a" . "📎")
        mu4e-headers-attach-mark          '("a" . "a")
        mu4e-headers-encrypted-mark       '("x" . "⚴")
        mu4e-headers-signed-mark          '("s" . "☡")
        mu4e-headers-unread-mark          '("u" . "⎕")
        mu4e-headers-fields               `((:date . 8)
                                            (:flags . 6)
                                            (:mailing-list . 10)
                                            (:from . 22)
                                            (:subject . ,(- (frame-width) (+ 8 6 10 22 8)))))

  (setf mu4e-bookmarks
        (list
         (make-mu4e-bookmark
          :name "Unread Messages"
          :query "flag:unread AND NOT flag:trashed"
          :key ?u)
         (make-mu4e-bookmark
          :name "Last 24 hours"
          :query "date:24h.."
          :key ?t)
         (make-mu4e-bookmark
          :name "Last 7 days"
          :query "date:7d..now"
          :key ?w)
         (make-mu4e-bookmark
          :name "Github Messages"
          :query "github"
          :key ?g)
         (make-mu4e-bookmark
          :name "Messages with images"
          :query "mime:image/*"
          :key ?p)))

  (setf mu4e-marks
        '((tag
           :char "t"
           :prompt "gtag"
           :ask-target
           (lambda nil (read-string "What tag do you want to add? "))
           :action
           (lambda (docid msg target) (mu4e-action-retag-message msg target)))

          (refile
           :char ("r" . "▶")
           :prompt "refile"
           :dyn-target
           (lambda (target msg) (mu4e-get-refile-folder msg))
           :action
           (lambda (docid msg target)
             (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))

          (delete
           :char ("D" . "❌ ")
           :prompt "Delete"
           :show-target (lambda (target) "delete")
           :action (lambda (docid msg target) (mu4e~proc-remove docid)))

          (flag
           :char ("+" . "⚑")
           :prompt "+flag"
           :show-target (lambda (target) "flag")
           :action
           (lambda (docid msg target)
             (mu4e~proc-move docid nil "+F-u-N")))

          (move
           :char ("m" . "▶")
           :prompt "move"
           :ask-target mu4e~mark-get-move-target
           :action (lambda (docid msg target)
                     (mu4e~proc-move docid
                                     (mu4e~mark-check-target target)
                                     "-N")))

          (read
           :char ("!" . "👁")
           :prompt "!read"
           :show-target (lambda (target) "read")
           :action (lambda (docid msg target)
                     (mu4e~proc-move docid nil "+S-u-N")))

          (trash
           :char ("d" . "🗑")
           :prompt "dtrash"
           :dyn-target (lambda (target msg)
                         (mu4e-get-trash-folder msg))
           :action (lambda (docid msg target)
                     (mu4e~proc-move docid
                                     (mu4e~mark-check-target target)
                                     "+T-N")))

          (unflag
           :char ("-" . "➖")
           :prompt "-unflag"
           :show-target (lambda (target) "unflag")
           :action (lambda (docid msg target)
                     (mu4e~proc-move docid nil "-F-N")))

          (untrash
           :char ("=" . "▲")
           :prompt "=untrash"
           :show-target (lambda (target) "untrash")
           :action (lambda (docid msg target)
                     (mu4e~proc-move docid nil "-T")))

          (unread
           :char "?"
           :prompt "?unread"
           :show-target (lambda (target) "unread")
           :action (lambda (docid msg target)
                     (mu4e~proc-move docid nil "-S+u-N")))

          (unmark
           :char " "
           :prompt "unmark"
           :action (mu4e-error "No action for unmarking"))

          (action
           :char ("a" . "◎")
           :prompt "action"
           :ask-target (lambda ()
                         (mu4e-read-option "Action: " mu4e-headers-actions))
           :action (lambda (docid msg actionfunc)
                     (save-excursion
                       (when (mu4e~headers-goto-docid docid)
                         (mu4e-headers-action actionfunc)))))

          (something
           :char ("*" . "✱")
           :prompt "*something"
           :action (mu4e-error "No action for deferred mark")))))

;; Functions
(std::after mu4e

  (make-process
   :name "Mbsync Update"
   :command '("systemctl" "--user" "start" "mbsync.service"))

  ;;WIP font-lock improvement
  (defun mu4e~headers-line-apply-flag-face (msg line) line)

  (defun mu4e~headers-field-apply-basic-properties (msg field val width)
    (cl-case field
      (:subject
       (propertize
        (concat
         (mu4e~headers-thread-prefix (mu4e-message-field msg :thread))
         (truncate-string-to-width val 600))
        'face
        (let ((flags (mu4e-message-field msg :flags)))
          (cond
           ((memq 'trashed flags) 'mu4e-trashed-face)
           ((memq 'draft flags) 'mu4e-draft-face)
           ((or (memq 'unread flags) (memq 'new flags))
            'mu4e-unread-face)
           ((memq 'flagged flags) 'mu4e-flagged-face)
           ((memq 'replied flags) 'mu4e-replied-face)
           ((memq 'passed flags) 'mu4e-forwarded-face)
           (t 'mu4e-header-face)))))
      (:thread-subject
       (propertize
        (mu4e~headers-thread-subject msg)
        'face 'font-lock-doc-face))
      ((:maildir :path :message-id)
       (propertize val 'face 'font-lock-keyword-face))
      ((:to :from :cc :bcc)
       (propertize
        (mu4e~headers-contact-str val)
        'face 'font-lock-variable-name-face))
      (:from-or-to (mu4e~headers-from-or-to msg))
      (:date
       (propertize
        (format-time-string mu4e-headers-date-format val)
        'face 'font-lock-string-face))
      (:mailing-list
       (propertize
        (mu4e~headers-mailing-list val)
        'face 'font-lock-builtin-face))
      (:human-date
       (propertize
        (mu4e~headers-human-date msg)
        'help-echo (format-time-string
                    mu4e-headers-long-date-format
                    (mu4e-msg-field msg :date))
        'face 'font-lock-string-face))
      (:flags
       (propertize (mu4e~headers-flags-str val)
                   'help-echo (format "%S" val)
                   'face 'font-lock-type-face))
      (:tags
       (propertize
        (mapconcat 'identity val ", ")
        'face 'font-lock-builtin-face))
      (:size (mu4e-display-size val))
      (t (mu4e~headers-custom-field msg field)))))

;; Keybinds
(std::after mu4e
  (std::keybind
    :keymap mu4e-main-mode-map
    "u" #'mu4e-update-index
    :evil motion mu4e-headers-mode-map
    "J"   #'std::evil::forward-five-lines
    "K"   #'std::evil::backward-five-lines
    "RET" #'mu4e-headers-view-message
    "gr"  #'std::mail::refresh
    "+"   #'std::mail::add-tag
    "-"   #'std::mail::remove-tag
    "!"   #'mu4e-view-mark-for-read
    :evil (normal motion) mu4e-main-mode-map
    "j" #'mu4e~headers-jump-to-maildir
    "b" #'mu4e-headers-search-bookmark
    :evil motion mu4e-view-mode-map
    "C-j" #'mu4e-view-headers-next
    "C-k" #'mu4e-view-headers-prev))
