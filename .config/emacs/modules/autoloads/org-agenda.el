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

(define-inline std::org::agenda::status-mark ()
  (declare (side-effect-free t))
  (inline-quote
   (if (org-get-scheduled-time (point))
       "›"
     (let ((tags (org-get-tags-at (point))))
       (cond
        ((member "next" tags) "+")
        ((member "wait" tags) "-")
        ((member "maybe" tags) "?")
        (t " "))))))

(pretty-hydra-define std::org::agenda::hydra

  (:color teal :quit-key "q" :title (concat (treemacs-get-icon-value 'calendar)
                                            (std::face "Agenda" 'font-lock-keyword-face)))

  (#("Allg." 0 5 (face font-lock-constant-face))
   (("a" (std::org::agenda-for-key "a") "2 Wochen")
    ("y" (std::org::agenda-for-key "y") "2 Wochen Ungefiltert")
    ("s" (std::org::agenda-for-key "s") "Inbox"))

   #("Privat" 0 6 (face font-lock-function-name-face))
   (("d" (std::org::agenda-for-key "d") "Kanban")
    ("f" (std::org::agenda-for-key "f") "Kategorien"))

   #("NT" 0 2 (face font-lock-type-face))
   (("j" (std::org::agenda-for-key "j") "Kunde")
    ("k" (std::org::agenda-for-key "k") "NT & Gilde")
    ("l" (std::org::agenda-for-key "l") "Kanban"))))

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

(defun std::org::agenda::schedule-now ()
  (interactive)
  (org-agenda-schedule nil (current-time)))

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
  (ht ("IDEA" 101)
      ("APPT" 100)
      ("LOOP" 99)
      ("STRY" 91)
      ("PROJ" 90)
      ("FRAGE"81)
      ("TIME" 80)
      ("NEXT" 70)
      ("TODO" 60)
      ("INFO" 55)
      ("BKMR" 50)
      ("WAIT" 40)))

(defun std::org::agenda::compare-by-todo-state (a b)
  (declare (side-effect-free t))
  (if (and (stringp a) (stringp b))
      (let* ((cat1  (get-text-property 0 'org-category a))
             (cat2  (get-text-property 0 'org-category b))
             (todo1 (get-text-property 0 'todo-state a))
             (todo2 (get-text-property 0 'todo-state b))
             (prio1 (get-text-property 0 'priority a))
             (prio2 (get-text-property 0 'priority b))
             (t1val (ht-get std::org::agenda::todo-priorities todo1 0))
             (t2val (ht-get std::org::agenda::todo-priorities todo2 0))
             (diff-todo (- t1val t2val))
             (diff-prio (- prio1 prio2)) )
        (if (string= cat1 cat2)
            (if (string= todo1 todo2)
                (cond
                 ((= 0 diff-prio) nil)
                 ((> diff-prio 0) +1)
                 (t -1))
              (cond
               ((= 0 diff-todo) nil)
               ((> diff-todo 0) +1)
               (t -1)))
          (or (and (string< cat1 cat2) 1) -1)))))

(defun std::org::agenda::open-link-at-line ()
  (interactive)
  (save-excursion
    (save-match-data
      (goto-char (next-single-char-property-change
                  (pos-bol)
                  'htmlize-link nil (pos-eol)))
      (when (eq 'org-link (get-text-property (point) 'face))
        (org-open-at-point)))))

(cl-defun std::org::agenda::roam-files-with-tags (&key in not-in)
  (require 'org-roam)
  (let ((in-clause
         (format "%s = (SELECT COUNT (*) FROM (SELECT tag FROM tags WHERE tags.node_id = nodes.id AND tags.tag IN (%s)))"
                 (length in)
                 (string-join
                  (--map (format "'\"%s\"'" it) in) ", ")))
        (not-in-clause
         (format "0 = (SELECT COUNT (*) FROM (SELECT tag FROM tags WHERE tags.node_id = nodes.id AND tags.tag IN (%s)))"
                 (string-join
                  (--map (format "'\"%s\"'" it) not-in) ", "))))
    (-uniq
     (-map
      #'car
      (org-roam-db-query
       (format
        "SELECT nodes.file FROM nodes WHERE %s"
        (cond
         ((and in not-in)
          (format "%s AND %s" in-clause not-in-clause))
         (in in-clause)
         (not-in not-in-clause)
         (t (error "Neither in nor not-in predicates are defined")))))))))

(defun std::org::agenda::format-date (date)
  (string-pad (concat " " (org-agenda-format-date-aligned date)) 90 ?\ ))

(define-inline std::org::agenda::extract-timestamp (pom)
  (declare (side-effect-free t))
  (inline-letevals (pom)
    (inline-quote
     (org-with-point-at ,pom
       (when (re-search-forward org-ts-regexp (org-entry-end-position) :no-error)
         (org-read-date nil nil (match-string 1)))))))

(define-inline std::org::agenda::show-time-left-tf (entry)
  (declare (side-effect-free t))
  (inline-letevals (entry)
    (inline-quote
     (let* ((now (ts-now))
            (str (if (listp ,entry) (car entry) ,entry))
            (org-marker (get-text-property 0 'org-marker str))
            (time-str (or (org-entry-get org-marker "SCHEDULED")
                          (std::org::agenda::extract-timestamp org-marker))))
       (when time-str
         (let* ((time (ts-parse-org time-str))
                (diff (ts-human-duration (ts-difference time now)))
                (days (plist-get diff :days))
                (hours (plist-get diff :hours))
                (len (length str))
                (offset (max 0 (- 40 len))))
           (format
            "%s %s%s"
            str
            (make-string offset ?\ )
            (std::face
             (cond
              ((> 0 days)
               "(Today)")
              ((= 0 days)
               (if (equal (ts-day now) (ts-day time))
                   (if (> 0 hours)
                       "(Today)"
                     (format "(%s hours)" hours))
                 "(1 day)"))
              ((= 1 days)
               (if (> (+ hours (ts-hour now)) 20)
                   "(2 days)"
                 "(1 day)") )
              (t
               (if (= 0 hours)
                   (format "(%s days)" days)
                 (format "(%s days)" (1+ days)))))
             'font-lock-comment-face))))))))

(defun std::org::agenda::extend-urgent (str)
  (concat str (make-string (max 0 (- 80 (length str))) ?\ )))

(defun std::org::agenda::toggle-agenda-tag ()
  (interactive)
  (org-agenda-set-tags
   (std::read "Tag:" '("wait" "next" "maybe" "daily" "retro"))))
