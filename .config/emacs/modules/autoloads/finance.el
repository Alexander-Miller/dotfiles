;; -*- lexical-binding: t -*-

(defvar std::ledger::saved-window-config nil)

(defconst std::ledger::month-separator-pattern (rx "### " (group-n 2 (1+ any)) " ###" eol))

(defconst std::ledger::months
  '((1 . "Januar")   (2 . "Februar")   (3 . "März")
    (4 . "April")    (5 . "Mai")       (6 . "Juni")
    (7 . "Juli")     (8 . "August")    (9 . "September")
    (10 . "Oktober") (11 . "November") (12 . "Dezember")))

(defun std::ledger ()
  (interactive)
  (require 'calendar)
  (let* ((year (cl-third (calendar-current-date)))
         (main-file (format "%s/Ledger.ledger" std::dirs::ledger))
         (year-file (format "%s/%s.ledger" std::dirs::ledger year)))
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

(defun std::ledger::file ()
  (interactive)
  (let* ((files (--map (cons (f-filename it) it)
                       (std::files std::dirs::ledger ".ledger")))
         (selection (completing-read "File: " files))
         (file (cdr (assoc selection files))))
    (find-file file)))

(defun std::ledger::mode-hook ()
  (outline-minor-mode)
  (evil-ledger-mode)
  (setq-local
   ledger-accounts-file     (format "%s/Ledger.ledger" std::dirs::ledger)
   outline-regexp           (rx bol "### ")
   imenu-generic-expression `(("Monat" ,std::ledger::month-separator-pattern 2))
   company-backends         '((company-capf :with company-yasnippet :separate) company-dabbrev)
   company-idle-delay       4))

(defun std::ledger::save ()
  "First `ledger-mode-clean-buffer', then `save-buffer'."
  (interactive)
  (-let [p (point)]
    (when (buffer-modified-p)
      (unwind-protect (ledger-mode-clean-buffer)
        (save-buffer)))
    (goto-char p)))

(defun std::ledger::finish ()
  "Kill all ledger buffers and restore saved window config."
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
        (if (search-forward (format "### %s" month) nil :no-error)
            (forward-line 1)
          (message "'%s' not found." month)
          (goto-char start))))))

(defun std::ledger::forward (&optional arg)
  "Go to the next transaction.
With a prefix ARG go to the next month."
  (interactive "P")
  (if arg
      (save-match-data
        (end-of-line)
        (search-forward-regexp
         std::ledger::month-separator-pattern
         nil :no-error))
    (call-interactively #'evil-ledger-forward-xact)))

(defun std::ledger::backward (&optional arg)
  "Go to the previous transaction.
With a prefix ARG go to the previous month."
  (interactive "P")
  (if arg
      (save-match-data
        (beginning-of-line)
        (search-backward-regexp
         std::ledger::month-separator-pattern
         nil :no-error))
    (call-interactively #'evil-ledger-backward-xact)))
