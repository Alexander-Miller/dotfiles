;; -*- lexical-binding: t -*-

(defun std::org::agenda::goto-today ()
  (interactive)
  (evil-goto-line)
  (std::schedule 0 :no-repeat
    (org-agenda-goto-today)))

(defun std::org::agenda::switch-to ()
  (interactive)
  (eyebrowse-switch-to-window-config (get #'org-agenda 'std::return-to-desktop))
  (org-agenda-switch-to))

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
