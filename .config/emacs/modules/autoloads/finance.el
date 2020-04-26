;; -*- lexical-binding: t -*-

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
    (setf std::ledger::save-window-config (current-window-configuration))
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
  (setq-local ledger-accounts-file (format "%s/Ledger.ledger" std::ledger-dir))
  (setq-local outline-regexp (rx bol "+++ "))
  (setq-local imenu-generic-expression `(("Monat" ,std::ledger::month-separator-pattern 2))))
