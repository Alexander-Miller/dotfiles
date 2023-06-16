;; -*- lexical-binding: t -*-

(std::using-packages
 ledger-mode
 evil-ledger)

(std::autoload finance
  #'std::ledger
  #'std::ledger::file
  #'std::ledger::report
  #'std::ledger::mode-hook
  #'std::ledger::save
  #'std::ledger::finish
  #'std::ledger::goto-current-month
  #'std::ledger::forward
  #'std::ledger::backward)

(defconst std::dirs::ledger (expand-file-name (format "%s/Ledger" std::dirs::org)))

(add-hook 'ledger-mode-hook #'std::ledger::mode-hook)
(add-hook 'ledger-report-after-report-hook #'fit-window-to-buffer)

(std::keybind
 :leader
 "all" #'std::ledger
 "alf" #'std::ledger::file
 "alr" #'std::ledger::report)

(std::after ledger-mode

  (put 'ledger-accounts-file 'safe-local-variable #'stringp)

  (std::add-transient-hook 'ledger-mode-hook
    (dolist (file (list (format "%s/utils/load-csv.el" std::dirs::ledger)
                        (format "%s/utils/show-budget.el" std::dirs::ledger)))
      (when (file-exists-p file) (load-file file))))

  (defface std::ledger::month-face
    '((t (:foreground "#ccb18b" :bold t :height 1.1 :background "#333366" :box (:line-width -1 :color "#1a1a1a") :extend t)))
    ""
    :group 'std)

  (font-lock-add-keywords
   'ledger-mode
   `((,(rx (group-n
            1
            bol
            "### "
            (1+ (or alnum " "))
            " ###"
            "\n"))
      1 'std::ledger::month-face t))
   'prepend)

  (std::pushnew ledger-report-format-specifiers
    (cons "current-year"
          (lambda () (format-time-string "%Y")))
    (cons "period"
          (lambda ()
            (-let [choice
                   (completing-read
                    "Period: "
                    '("this year"
                      "last year"
                      "this month"
                      "all"
                      "custom"))]
              (pcase choice
                ("all" "from 2010 until this day")
                ("custom" (read-string "Period: "))
                (other other))))))


  (setq-default ledger-master-file (expand-file-name "Ledger.ledger" std::dirs::ledger))

  (setf
   ledger-default-date-format           ledger-iso-date-format
   ledger-mode-should-check-version     nil
   ledger-post-amount-alignment-column  62
   ledger-post-account-alignment-column 2
   ledger-clear-whole-transactions      t
   ledger-reports
   `(("Budget"
      ,(format "python %s/utils/budget-report.py" std::dirs::ledger))
     ("Invest"
      ,(format "emacs -batch -l %s/utils/invest-report.el" std::dirs::ledger))
     ("Register (real)"
      "%(binary) reg %(account) --real -p %(period)")
     ("Register (+virtuell)"
      "%(binary) reg %(account) -p %(period)")
     ("Guthaben (EUR)"
      "%(binary) bal ^Guthaben -X EUR")
     ("Guthaben (alle Währungen)"
      "%(binary) bal Guthaben")
     ("Kredit"
      "%(binary) bal \"/(Ausgaben:Kreditzahlung|Last:Wohnungskredit)/\" --no-total"))))

(std::after ledger-mode
  (std::keybind
    :mode-leader ledger-mode
    "C-w" #'std::ledger::finish
    "c"   #'std::ledger::goto-current-month
    "L"   #'std::ledger::parse-csv
    "s"   #'ledger-sort-buffer
    "S"   #'ledger-sort-region
    "o"   #'ledger-occur-mode
    "y"   #'ledger-copy-transaction-at-point
    "d"   #'ledger-delete-current-transaction
    "r"   #'ledger-report
    "R"   #'ledger-reconcile
    :keymap ledger-mode-map
    "M-J"   #'std::ledger::forward
    "M-K"   #'std::ledger::backward
    "M-q"   #'ledger-post-align-dwim
    [remap save-buffer] #'std::ledger::save))
