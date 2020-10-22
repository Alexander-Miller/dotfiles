;; -*- lexical-binding: t -*-

(defun std::org::agenda ()
  (interactive)
  (--if-let (get-buffer "*Org Agenda*")
      (call-interactively #'org-agenda)
    (std::org::agenda::hydra/body)))

(defun std::org::agenda::forced-select ()
  (interactive)
  (std::org::agenda::hydra/body))

(defun std::org::agenda-for-key (key)
  (--when-let (get-buffer "*Org Agenda*") (kill-buffer it))
  (-let [inhibit-message t]
    (org-agenda nil key)))

(pretty-hydra-define std::org::agenda::hydra

  (:color teal :quit-key "q" :title (concat (treemacs-get-icon-value 'calendar)
                                            (std::face "Agenda" 'font-lock-keyword-face)))

  (#("Allg." 0 5 (face font-lock-constant-face))
   (("a" (std::org::agenda-for-key "a") "2 Wochen")
    ("s" (std::org::agenda-for-key "s") "Inbox"))

   #("Privat" 0 6 (face font-lock-function-name-face))
   (("d" (std::org::agenda-for-key "d") "Kanban")
    ("f" (std::org::agenda-for-key "f") "Kategorien"))

   #("NT" 0 2 (face font-lock-type-face))
   (("j" (std::org::agenda-for-key "j") "Kunde")
    ("k" (std::org::agenda-for-key "k") "NT & AQE & AEP"))))

(defun std::org::agenda::goto-today ()
  (interactive)
  (evil-goto-line)
  (std::schedule 0 :no-repeat
    (org-agenda-goto-today)))

(defun std::org::agenda::switch-to ()
  (interactive)
  (eyebrowse-switch-to-window-config 1)
  (org-agenda-switch-to))

(defun std::org::agenda::quit ()
  (interactive)
  (eyebrowse-switch-to-window-config 1))

(defun std::org::agenda::unschedule ()
  (interactive)
  (org-agenda-schedule '(4)))

(defun std::org::agenda::mark-habits ()
  "https://emacs.stackexchange.com/a/17328/16972"
  (when (not (get-text-property (point) 'org-series))
    (let ((cursor (point))
          (item)
          (data))
      (while (setf cursor (next-single-property-change cursor 'org-marker))
        (setf item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setf data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

(defconst std::org::agenda::todo-priorities
  '(("INBOX" . 101)
    ("APPT"  . 100)
    ("HABIT" . 99)
    ("STORY" . 91)
    ("PROJ"  . 90)
    ("FRAGE" . 81)
    ("TASK"  . 80)
    ("NEXT"  . 70)
    ("TODO"  . 60)
    ("MAYBE" . 50)
    ("WAIT"  . 40)))

(defun std::org::agenda::compare-by-todo-state (a b)
  (declare (side-effect-free t))
  (let* ((todo1 (get-text-property 0 'todo-state a))
         (todo2 (get-text-property 0 'todo-state b))
         (prio1 (alist-get todo1 std::org::agenda::todo-priorities 0 nil #'string=))
         (prio2 (alist-get todo2 std::org::agenda::todo-priorities 0 nil #'string=))
         (diff (- prio1 prio2)))
    (cond
     ((= 0 diff) nil)
     ((> diff 0) +1)
     (t          -1))))

(defmacro std::org::agenda::now-plus (amount unit)
  (let ((slot
         (pcase unit
           (`hours 'hours)
           (`days  'day)
           (`weeks 'week)
           (other  (error "Unknown unit '%s'" other)))))
    `(ts-format "%F %T" (ts-inc ',slot ,amount (ts-now)))))
