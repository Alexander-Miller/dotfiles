;; -*- lexical-binding: t -*-

(defvar std::ledger::saved-window-config nil)

(defconst std::ledger::month-separator-pattern (rx "+++ " (group-n 2 (1+ any)) " +++" eol))

(defconst std::ledger-dir (expand-file-name (format "%s/Ledger" std::org-dir)))

(defconst std::ledger::months
  '((1 . "Januar")   (2 . "Februar")   (3 . "März")
    (4 . "April")    (5 . "Mai")       (6 . "Juni")
    (7 . "Juli")     (8 . "August")    (9 . "September")
    (10 . "Oktober") (11 . "November") (12 . "Dezemper")))

(defun std::ledger ()
  (interactive)
  (require 'calendar)
  (-let [pcmds (format "%s/ledger.el" std::ledger-dir)]
    (when (file-exists-p pcmds)
      (load-file pcmds)))
  (let* ((date (calendar-current-date))
         (year (cl-third (calendar-current-date)))
         (main-file (format "%s/Ledger.ledger" std::ledger-dir))
         (year-file (format "%s/%s.ledger" std::ledger-dir year)))
    (setf std::ledger::saved-window-config (current-window-configuration))
    (delete-other-windows)
    (find-file year-file)
    (split-window-right)
    (save-selected-window
      (-let [full-width (* 2 (window-width))]
        (shrink-window-horizontally (- (/ full-width 7))))
      (other-window 1)
      (find-file main-file))
    (std::ledger::goto-current-month)))

(defun std::ledger::mode-hook ()
  (outline-minor-mode)
  (evil-ledger-mode)
  (std::spellcheck::use-de-dict)
  (setq-local ledger-accounts-file (format "%s/Ledger.ledger" std::ledger-dir))
  (setq-local outline-regexp (rx bol "+++ "))
  (setq-local imenu-generic-expression `(("Monat" ,std::ledger::month-separator-pattern 2))))

(defun std::ledger::save ()
  "First `ledger-mode-clean-buffer', then `save-buffer'."
  (interactive)
  (-let [p (point)]
    (when (buffer-modified-p)
      (unwind-protect (ledger-mode-clean-buffer)
        (save-buffer)))
    (goto-char p)))

(defun std::ledger::finish ()
  (interactive)
  (cl-loop
   for buf in (buffer-list)
   if (eq 'ledger-mode (buffer-local-value 'major-mode buf)) do
   (with-current-buffer buf
     (when (buffer-file-name)
       (save-buffer)
       (kill-buffer))))
  (when std::ledger::saved-window-config
    (set-window-configuration std::ledger::saved-window-config)))

(defun std::ledger::goto-current-month ()
  (interactive)
  (-let [month (-> (calendar-current-date)
                   (car)
                   (alist-get std::ledger::months))]
    (save-match-data
      (-let [start (point)]
        (goto-char 0)
        (if (search-forward (format "+++ %s" month) nil :no-error)
            (forward-line 1)
          (message "'%s' not found." month)
          (goto-char start))))))

(defun std::ledger::forward ()
  (interactive)
  (if (s-matches? std::ledger::month-separator-pattern
                  (thing-at-point 'line))
      (save-match-data
        (end-of-line)
        (search-forward-regexp std::ledger::month-separator-pattern nil :no-error))
    (call-interactively #'evil-ledger-forward-xact)))

(defun std::ledger::backward ()
  (interactive)
  (if (s-matches? std::ledger::month-separator-pattern
                  (thing-at-point 'line))
      (save-match-data
        (beginning-of-line)
        (search-backward-regexp std::ledger::month-separator-pattern nil :no-error))
    (call-interactively #'evil-ledger-backward-xact)))
